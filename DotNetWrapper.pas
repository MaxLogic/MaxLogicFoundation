unit DotNetWrapper;
{version: 3}
interface
uses
  windows, sysUtils, classes, jclDotNet, Variants;


type
{$IFNDEF UNICODE}
UnicodeString = WideString;
{$ENDIF}

  TDotNetWrapper = class(TObject)
  private
    fHost : TJclClrHost;
    fAssembly : OleVariant;
  protected

  public
    constructor Create(const AsemblyFileName, FullClassName, ClrVersionString : string);
    destructor Destroy; override;

    Property Assembly : OleVariant read fAssembly;

  end;

implementation
uses
  mscorlib_TLB;

{ TDotNetWrapper }


// For Example
// AsemblyFileName - should be the full file name with path to the assembly
// FullClassName - classname including full NameSpace
constructor TDotNetWrapper.Create;
var
  wsClrVersionString: UnicodeString;
  s: string;
  obj: _ObjectHandle;
  ad: TJclClrAppDomain;
begin
  wsClrVersionString := ClrVersionString;
  inherited Create;

  //create and run host
  fHost := TJclClrHost.Create(wsClrVersionString);
  fHost.Start;



  //create domain
  ad := fHost.CreateAppDomain('myNET');

  //locate the object (TFormTestClass) handle
  obj := (ad as _AppDomain).CreateInstanceFrom(AsemblyFileName, FullClassName );

  //get the target object
  fAssembly := obj.Unwrap;
end;

destructor TDotNetWrapper.Destroy;
begin
  //stop and cleanup the fHost
  fhost.Stop;
  fhost.Free;


  inherited;
end;





end.

