unit MaxLogic.CommandLineActionDispatcher;

{
  Version: 1.24

  uses and expands on the GpCommandLineParser, see:
  https://www.thedelphigeek.com/2014/12/attribute-based-command-line-parsing.html
  https://github.com/gabr42/GpDelphiUnits

  it is primary intended for CLI tools that accept a command / action as the first parameter which will be followed by options specific to that command

  Names used here:
  Action - this is the first argument in the command line prompt
  $ cmd.exe MyAction -Switch1
  Arguments - command line switches or parameters. All except the first which we are calling here the Action
  Service - the class that implements the logic needed to execute the action.

  This small Framework is inspired by Microservices architecture or the various web server dispatcher
  Simply declare a Service like a normal class,
  mark the method that should be called when the Action is indeed the first command line argument with the [CLPAction('ActionName')] atribute, like this:

  TMyService = class
  public
  [CLPAction('run,Execute'),
  CLPDescription('my action description')]
  Procedure Execute(aArgs: TMyArgs);
  end;

  valid is also a method without any parameters

  TMyService = class
  public
  [CLPAction('run,Execute')]
  Procedure Execute;
  end;

  now register it in the dispatcher
  TCLPActionDispatcher .Default.Register(TMyService);

  now when you execute your application like this:
  $ myAppexe run -arg1 -arg2
  or
  $ myAppexe execute -arg1 -arg2
  the dispatcher will create an instance of TMyArgs,
  it will call the gpCommandParser to populate it with values
  then the dispatcher will create an instance of TMyService and invoke the execute method of that class passing the TMyArgs as the single parameter
  after all is done, both instances will be destroyed

  in case your wonder, "CL" stands for Command Line
}

interface

uses
  System.SysUtils,
  System.TypInfo,
  System.RTTI,
  System.Classes,
  System.Generics.Defaults,
  System.Generics.Collections,
  GpCommandLineParser;

type

  TCLActionDispatcher = class
  private
    class var fInstance: TCLActionDispatcher;
    Class Constructor CreateClass;
    class Destructor DestroyClass;
  private type
    TActionKind = (akNormal, akSimple);

    TCLPActionEntry = class
      kind: TActionKind; // should we ever need more then those 2 kinds... then we probably should split this class up inheriting from the same base class, but for now, that is simpler
      // comma separated list
      ActionNames: String;
      MethodName: String;
      ArgsParam: TRttiType;
      ServiceClass: TClass;
      Description: String;
      SimpleProc: TProc;
      // ActionNames contains a comma separated list of action names,
      // where the first is the main name and the others are aliases,
      // but sometimes we need just the first one
      Function FirstActionName: String;
    end;
  private
    // key is the lowercased action name, there may be multipe same entries here if the action has multiple aliases
    fActionDic: TDictionary<String, TCLPActionEntry>;
    fActionList: TObjectList<TCLPActionEntry>;
    fDescription: String;
    Function CreateArgsInstance(aEntry: TCLPActionEntry; out aArgs: Tobject): Boolean;
    // if called with an empty aCommandLine it will parse the arguments from the command line, otherwise it wil use the aCommandLine
    function DoExecute(const aCommandLine: String = ''): Boolean;
    // without the first argumet, so we skip the action itself
    Function GetAdjustedCommandLine: String;
  public
    Constructor Create;
    Destructor Destroy; override;

    Procedure Register(aServiceClass: TClass); overload;
    // for a simpler approach just with a action and a method to be executed
    // actionNames is a comma separated list of names for the action, so you can define some aliases for the same command
    Procedure Register(const aActionNames: String; aProc: TProc; const aDescription: String = '')overload;

    Procedure OutputUsageToConsole;
    procedure OutputUsageToStrings(aUsage: TStrings);

    Function Execute: Boolean; overload;
    Function Execute(const aCommandLine: String): Boolean; overload;

    class property Default: TCLActionDispatcher read fInstance;
    /// <summary>
    /// Description property - provide a description for your command line tool
    /// </summary>
    property Description: String read fDescription write fDescription;
  end;

  /// <summary>
  /// Command Line Parser Action
  /// You have to mark your Action Method with this attribute where the ActionName must be the first CL Argument
  /// </summary>
  CLPActionAttribute = class(TCustomAttribute)
  private
    fActionName: String;
  public
    constructor Create(const aActionName: String);
    property ActionName: String read fActionName;
  end;

implementation

uses
  System.StrUtils,
  MaxLogic.RTTIHelper,
  MaxLogic.StrUtils,
  autoFree,
  MaxLogic.ioUtils;

{ TCLPActionDispatcher }

constructor TCLActionDispatcher.Create;
begin
  inherited Create;

  fActionDic := TDictionary<String, TCLPActionEntry>.Create;
  fActionList := TObjectList<TCLPActionEntry>.Create;
end;

function TCLActionDispatcher.CreateArgsInstance(aEntry: TCLPActionEntry;
  out aArgs: Tobject): Boolean;
begin
  Result := false;
  if aEntry.kind <> akNormal then
    Exit;
  if aEntry.ArgsParam <> nil then
  begin
    aArgs := TRTTIHelper.CreateClassInstanceFromRttiType(aEntry.ArgsParam);
    Result := True;
  end;

end;

class constructor TCLActionDispatcher.CreateClass;
begin
  fInstance := TCLActionDispatcher.Create;
end;

destructor TCLActionDispatcher.Destroy;
begin
  fActionDic.Free;
  fActionList.Free;
  inherited;
end;

class destructor TCLActionDispatcher.DestroyClass;
begin
  FreeAndNil(fInstance);
end;

Function TCLActionDispatcher.Execute: Boolean;
begin
  Result := DoExecute;
end;

Function TCLActionDispatcher.DoExecute(const aCommandLine: String = ''): Boolean;
var
  lActionName: String;
  lEntry: TCLPActionEntry;
  cp: IGpCommandLineParser;
  lArgs: Tobject;
  lService: Tobject;
  lCommandLine: String;
  i: Integer;
begin
  Result := false;
  lCommandLine := aCommandLine;
  if lCommandLine = '' then
  begin
    if ParamCount = 0 then
      Exit(false);
    lActionName := ParamStr(1);
  end else begin
    i := pos(' ', lCommandLine);
    if i < 1 then
    begin
      lActionName := trim(lCommandLine);
      lCommandLine := '';
    end else begin
      lActionName := trim(copy(lCommandLine, 1, i - 1));
      lCommandLine := trim(copy(lCommandLine, i + 1, length(lCommandLine)));
    end;
  end;

  if fActionDic.TryGetValue(ansiLowercase(lActionName), lEntry) then
  begin
    case lEntry.kind of
      akSimple:
        begin
          if assigned(lEntry.SimpleProc) then
          begin
            lEntry.SimpleProc();
            Result := True;
          end;
        end;
      akNormal:
        begin
          // init variables, so we know if we need to free them in try finally
          lArgs := nil;
          lService := nil;
          try
            // is the action method without parameters or do we need to parse the command line and pass the arguments to the method?
            if (lEntry.ArgsParam = nil) then
            begin
              Result := True;
            end else begin
              CreateArgsInstance(lEntry, lArgs);
              cp := CreateCommandLineParser;
              if lCommandLine <> '' then
                Result := cp.Parse(lCommandLine, lArgs)
              else
                Result := cp.Parse(GetAdjustedCommandLine, lArgs);
            end;

            if Result then
            begin
              lService := TRTTIHelper.CreateInstanceFromTClass(lEntry.ServiceClass);
              if assigned(lArgs) then
                TRTTIHelper.Call(lService, lEntry.MethodName, [lArgs])
              else
                TRTTIHelper.Call(lService, lEntry.MethodName, []);
            end;
          finally
            FreeAndNil(lArgs);
            FreeAndNil(lService);
          end;
        end;
    end;
  end;

end;

Function TCLActionDispatcher.Execute(const aCommandLine: String): Boolean;
begin
  if aCommandLine = '' then
    Result := false
  else
    Result := DoExecute(aCommandLine);
end;

function TCLActionDispatcher.GetAdjustedCommandLine: String;
var
  i: Integer;
begin
  Result := '';
  for i := 2 to ParamCount do
  begin
    if Result <> '' then
      Result := Result + ' ';
    if pos(' ', ParamStr(i)) > 0 then
      Result := Result + '"' + ParamStr(i) + '"'
    else
      Result := Result + ParamStr(i);
  end;
end;

procedure TCLActionDispatcher.OutputUsageToConsole;
var
  l: TStringList;
  x: Integer;
begin
  gc(l, TStringList.Create);
  OutputUsageToStrings(l);
  for x := 0 to l.Count - 1 do
    writeLn(l[x]);
end;

procedure TCLActionDispatcher.OutputUsageToStrings(aUsage: TStrings);
var
  cp: IGpCommandLineParser;
  lEntry: TCLPActionEntry;
  s: String;
  ar: TArray<TCLPActionEntry>;
  lNames: TArray<String>;
  lArgs: Tobject;
  first: Boolean;
begin
  lArgs := nil;
  aUsage.Add(ExtractFilename(ParamStr(0)) + ' ' + MaxLogic.ioUtils.GetVersionString);
  aUsage.Add('');
  if self.Description <> '' then
  begin
    aUsage.Add(self.Description);
    aUsage.Add('');
  end;
  aUsage.Add('Parameters can be introduced by /, - and --. Values can be separated from parameter ' +
      'names by : or =. When a short (single letter) name is used, separator is not required');
  aUsage.Add('');

  ar := fActionList.ToArray;
  TArray.Sort<TCLPActionEntry>(
    ar, TComparer<TCLPActionEntry>.Construct(
        function(const Left, Right: TCLPActionEntry): Integer
    begin
      Result := TComparer<String>.Default.Compare(Left.FirstActionName, Right.FirstActionName);
    end));

  for lEntry in ar do
  begin
    lNames := lEntry.ActionNames.Split([',']);
    aUsage.Add('Action: ' + lNames[0]);
    if length(lNames) > 1 then
    begin
      aUsage.Add('Action aliases: ' +
        String.join(', ', lNames, 2, length(lNames) - 1));
    end;
    if lEntry.Description <> '' then
      aUsage.Add('Description: ' + lEntry.Description);

    case lEntry.kind of
      akNormal:
        begin
          Try
            if CreateArgsInstance(lEntry, lArgs) then
            begin
              cp := CreateCommandLineParser;
              cp.Parse(lArgs);
              first := True;
              for s in cp.Usage do
              begin
                if first then
                begin
                  aUsage.Add('Parameters / Options:');
                  first := false;
                end else if s <> '' then
                  aUsage.Add('  ' + s);
              end;
            end;
          Finally
            FreeAndNil(lArgs);
          End;
        end;
    end;

    aUsage.Add(''); // leave empty line between the commands
  end;
end;

procedure TCLActionDispatcher.Register(const aActionNames: String;
aProc: TProc; const aDescription: String = '');
var
  lActionName: String;
  lEntry: TCLPActionEntry;
begin
  lEntry := TCLPActionEntry.Create;
  lEntry.ActionNames := aActionNames;
  lEntry.Description := aDescription;
  lEntry.kind := akSimple;
  lEntry.SimpleProc := aProc;
  fActionList.Add(lEntry);
  for lActionName in lEntry.ActionNames.Split([',']) do
    fActionDic.Add(ansiLowercase(lActionName), lEntry)

end;

procedure TCLActionDispatcher.Register(aServiceClass: TClass);
var
  lActionAttribute: CLPActionAttribute;
  lMethodName: String;
  lActionName: String;
  lEntry: TCLPActionEntry;
  lRttiMethod: TRttiMethod;
  lParams: TArray<TRttiParameter>;
  lArg: TRttiType;
begin
  if not TRTTIHelper.FindMethodByAttribute<CLPActionAttribute>(aServiceClass, lMethodName, lActionAttribute, lRttiMethod) then
    raise Exception.Create(ClassName + '.Register: ' + aServiceClass.ClassName + ' must have the ' + CLPActionAttribute.ClassName + ' attribute');

  lParams := lRttiMethod.GetParameters;
  case length(lParams) of
    0:
      lArg := nil;
    1:
      begin
        if lParams[0].ParamType.TypeKind <> tkClass then
          raise Exception.Create(ClassName + '.Register: Invalid Parameter type for ' + aServiceClass.ClassName + '.' + lMethodName + '. It may have either no parameters or one parameter descending from TObject');
        lArg := lParams[0].ParamType;
      end;
  else
    raise Exception.Create(ClassName + '.Register: Invalid number of Parameters for ' + aServiceClass.ClassName + '.' + lMethodName + '. It may have either no parameters or one parameter descending from TObject');
  end;

  lEntry := TCLPActionEntry.Create;
  lEntry.kind := akNormal;

  if lRttiMethod.hasAttribute<CLPDescriptionAttribute> Then
    lEntry.Description := lRttiMethod.GetAttribute<CLPDescriptionAttribute>.Description;

  lEntry.ActionNames := lActionAttribute.ActionName;
  lEntry.ServiceClass := aServiceClass;
  lEntry.MethodName := lMethodName;
  lEntry.ArgsParam := lArg;

  fActionList.Add(lEntry);
  for lActionName in lEntry.ActionNames.Split([',']) do
  begin
    fActionDic.Add(ansiLowercase(trim(lActionName)), lEntry);
  end;
end;

{ CLPActionAttribute }

constructor CLPActionAttribute.Create(const aActionName: String);
begin
  inherited Create;
  fActionName := aActionName;
end;

{ TCLPActionDispatcher.TCLPActionEntry }

function TCLActionDispatcher.TCLPActionEntry.FirstActionName: String;
begin
  if ActionNames = '' then
    Result := ''
  else
    Result := ActionNames.Split([','], 1)[0];
end;

end.
