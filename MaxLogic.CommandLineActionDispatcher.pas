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
  // raw form of the usage information
  // use one of the formatters to output it into a different format
  TRawCommandHelp = record
    Names: TArray<String>; // names of the actions
    Description: String;
    CommandLineParser: IGpCommandLineParser;
  end;

  TUsageRawhelp = record
    Title: String; // contains exe name + version
    Version: String; // contains just the version of the exe
    Description: String;
    ParamUsageInfo: String;
    Commands: TArray<TRawCommandHelp>;
  end;

  TPlainTextHelpFormatter = class
  public
    // set aWrapAtColumn=0 to disable line wrapping
    class procedure Execute(const aRaw: TUsageRawhelp; aOutputUsage: TStrings; aWrapAtColumn: Integer = 80);
  end;

  TMarkDownHelpFormatter = class
  private
    procedure DoExec(const aRaw: TUsageRawhelp; aOutputUsage: TStrings);
    function preProcessParamNames(const aValue: String; var aMaxLen: Integer): String;
    function preProcessParamDesc(const aValue: String; var aMaxLen: Integer): String;
    function writeCell(const aText: String; aChar: Char; aLen: Integer): String;
  public
    // set aWrapAtColumn=0 to disable line wrapping
    class procedure Execute(const aRaw: TUsageRawhelp; aOutputUsage: TStrings);
  end;

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
    // set wrapAtColumn=0 to disable line breaks
    procedure OutputUsageToStrings(aUsage: TStrings; aWrapAtColumn: Integer = 80);
    function OutputUsage: TUsageRawhelp;

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

procedure TCLActionDispatcher.OutputUsageToStrings(aUsage: TStrings; aWrapAtColumn: Integer = 80);
var
  raw: TUsageRawhelp;
begin
  raw := OutputUsage;
  TPlainTextHelpFormatter.Execute(raw, aUsage, aWrapAtColumn);
end;

function TCLActionDispatcher.OutputUsage: TUsageRawhelp;
const
  br = sLineBreak;
var
  i: Integer;
  cp: IGpCommandLineParser;
  lEntry: TCLPActionEntry;
  ar: TArray<TCLPActionEntry>;
  lArgs: Tobject;
begin
  Result := System.default(TUsageRawhelp);

  lArgs := nil;
  Result.Version := MaxLogic.ioUtils.GetVersionString;
  Result.Title := ExtractFilename(ParamStr(0)) + ' ' + Result.Version;
  Result.Description := Description;

  Result.ParamUsageInfo :=
    'Parameters can be introduced by `/`, `-` and `--`.' + br +
    'Values can be separated from parameter names by `:` or `=`.' + br +
    'When a short (single letter) name is used, a separator is not required';

  ar := fActionList.ToArray;
  TArray.Sort<TCLPActionEntry>(
    ar, TComparer<TCLPActionEntry>.Construct(
        function(const Left, Right: TCLPActionEntry): Integer
    begin
      Result := TComparer<String>.default.Compare(Left.FirstActionName, Right.FirstActionName);
    end));

  setLength(Result.Commands, length(ar));
  i := 0;
  for lEntry in ar do
  begin
    Result.Commands[i].Names := lEntry.ActionNames.Split([',']);
    Result.Commands[i].Description := lEntry.Description;

    case lEntry.kind of
      akNormal:
        begin
          Try
            if CreateArgsInstance(lEntry, lArgs) then
            begin
              cp := CreateCommandLineParser;
              cp.Parse(lArgs);
              Result.Commands[i].CommandLineParser := cp;
            end;
          Finally
            FreeAndNil(lArgs);
          End;
        end;
    end; // case

    inc(i); // next element
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

{ TPlainTextHelpFormatter }

class procedure TPlainTextHelpFormatter.Execute(const aRaw: TUsageRawhelp;
aOutputUsage: TStrings; aWrapAtColumn: Integer);
const
  br = sLineBreak;
var
  l: TStrings;
  lCommand: TRawCommandHelp;
  cp: IGpCommandLineParser;
  lFirst: Boolean;
  s: String;
begin
  l := aOutputUsage;

  l.Add(aRaw.Title + br);

  if aRaw.Description <> '' then
    l.Add(aRaw.Description + br);
  l.Add(aRaw.ParamUsageInfo + br);

  for lCommand in aRaw.Commands do
  begin
    if length(lCommand.Names) <> 0 then
    begin
      l.Add('Action: ' + lCommand.Names[0]);
      if length(lCommand.Names) > 1 then
      begin
        l.Add('Action aliases: ' +
          String.join(', ', lCommand.Names, 2, length(lCommand.Names) - 1));
      end;
    end;

    if lCommand.Description <> '' then
      l.Add('Description: ' + lCommand.Description);

    cp := lCommand.CommandLineParser;
    if cp <> nil then
    begin
      lFirst := True;
      if aWrapAtColumn <> 0 then
        dec(aWrapAtColumn, 2); // actually we are adding an additional ident of 2 spaces, so we need to ask the parser to take this into account and break 2 chars erlier
      for s in cp.Usage(aWrapAtColumn) do
      begin
        if lFirst then // we want skip the First item in the list of the cp.Usage list
        begin
          l.Add('Parameters / Options:');
          lFirst := false;
        end else if s <> '' then
          l.Add('  ' + s);
      end;
    end;
    l.Add(''); // leave empty line between the commands
  end;
end;

{ TMarkDownHelpFormatter }

class procedure TMarkDownHelpFormatter.Execute(const aRaw: TUsageRawhelp;
aOutputUsage: TStrings);
var
  lFormater: TMarkDownHelpFormatter;
begin
  gc(lFormater, TMarkDownHelpFormatter.Create);
  lFormater.DoExec(aRaw, aOutputUsage);
end;

function TMarkDownHelpFormatter.preProcessParamDesc(const aValue: String;
var aMaxLen: Integer): String;
var
  ar: TArray<String>;
  s: String;
  x: Integer;
  lLen: Integer;
begin
  s := aValue;
  s := StringReplace(s, sLineBreak, #10, [rfReplaceAll]);
  ar := s.Split([#10]);
  for x := 0 to length(ar) - 1 do
  begin
    lLen := length(ar[x]);
    if (x <> length(ar) - 1) then
      ar[x] := trim(ar[x]) + '<br>';
    if lLen > aMaxLen then
      aMaxLen := lLen;
  end;
  Result := String.join('', ar);
end;

function TMarkDownHelpFormatter.preProcessParamNames(const aValue: String;
var aMaxLen: Integer): String;
var
  ar: TArray<String>;
  lIsOptional: Boolean;
  s: String;
  x: Integer;
begin
  s := aValue;

  lIsOptional := false;
  if s[1] = '[' then
  begin
    lIsOptional := True;
    s := copy(s, 2, length(s) - 2);
  end;

  ar := s.Split([',']);
  for x := 0 to length(ar) - 1 do
    ar[x] := '`' + trim(ar[x]) + '`';
  Result := String.join(', ', ar);
  if lIsOptional then
    Result := '[' + Result + ']';

  if length(Result) > aMaxLen then
    aMaxLen := length(Result);

end;

function TMarkDownHelpFormatter.writeCell(const aText: String; aChar: Char;
aLen: Integer): String;
var
  i: Integer;
begin
  Result := aText;
  i := aLen - length(Result);
  if i > 0 then
    Result := Result + StringofChar(aChar, i);
end;

procedure TMarkDownHelpFormatter.DoExec(const aRaw: TUsageRawhelp; aOutputUsage: TStrings);
const
  br = sLineBreak;
var
  l: TStrings;
  x, i: Integer;
  lFirst: Boolean;
  lCommand: TRawCommandHelp;
  cp: IGpCommandLineParser;
  s: String;
  lFormatter: TMarkDownHelpFormatter;
  lRows: TList<TArray<String>>;
  lRow: TArray<String>;
  lMaxParamNameLen, lMaxParamDescLineLen: Integer;
begin
  gc(lRows, TList < TArray < String >>.Create);
  l := aOutputUsage;

  l.Add('# ' + aRaw.Title + br);

  if aRaw.Description <> '' then
    l.Add(aRaw.Description + br);
  l.Add(aRaw.ParamUsageInfo + br);

  for lCommand in aRaw.Commands do
  begin
    if length(lCommand.Names) <> 0 then
    begin
      l.Add('## ' + lCommand.Names[0]);
      if length(lCommand.Names) > 1 then
        l.Add('Action aliases: ' + String.join(', ', lCommand.Names, 1, length(lCommand.Names) - 1) + br);
    end;

    if lCommand.Description <> '' then
      l.Add(lCommand.Description);

    lRows.Clear;
    cp := lCommand.CommandLineParser;
    if cp <> nil then
    begin
      lFirst := True;
      for s in cp.Usage(0) do
      begin
        if lFirst then // we want skip the First item in the list of the cp.Usage list
        begin
          lFirst := false;
          lRows.Add(['Parameter', 'Description']);
          lMaxParamNameLen := length(lRows[0][0]);
          lMaxParamDescLineLen := length(lRows[0][1]);
          continue;
        end;

        if s <> '' then
        begin
          i := pos(' - ', s);
          lRows.Add([
            preProcessParamNames(trim(copy(s, 1, i - 1)), lMaxParamNameLen),
            preProcessParamDesc(trim(copy(s, i + 3, length(s))), lMaxParamDescLineLen)]);
        end;
      end;

      // now write them
      l.Add('');
      for x := 0 to lRows.Count - 1 do
      begin
        lRow := lRows[x];
        s := '| ' + writeCell(lRow[0], ' ', lMaxParamNameLen) + ' | ' + writeCell(lRow[1], ' ', lMaxParamDescLineLen) + ' |';
        l.Add(s);

        if x = 0 then
          l.Add(
          '| ' + writeCell('---', '-', lMaxParamNameLen) + ' | ' + writeCell('---', '-', lMaxParamDescLineLen) + ' |');
      end;
      l.Add(''); // leave empty line between the commands
    end;
  end;
end;

end.
