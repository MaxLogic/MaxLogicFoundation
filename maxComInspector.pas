{ *******************************************************


  Digital Omerta 3D Engine

  Unit Author:   Pawel Piotrowski
  Copyright 	    Pawel Piotrowski
  ******************************************************* }

unit maxCOMInspector;
{$I JEDI.INC}
interface

Uses
  Classes, SysUtils, Windows, Variants, ComObj, ActiveX, ObjComAuto;

CONST
  error_invalid_typecast = 'Invalid Type Cast';
ERROR_DISPATCH_IS_NULL = 'ERROR_DISPATCH_IS_NULL';
ERROR_UNKNOWN_METHODNAME = 'ERROR_FUNCTIONNOTFOUND';
ERROR_EXTERNALEXCEPTION = 'ERROR_EXTERNALEXCEPTION';
    ERROR_SysErrorMessage = 'EERROR_SYSERRORMESSAGE';
Type
{$IFNDEF UNICODE}
  Unicodestring = WideString;
{$ELSE}

{$ENDIF}

  TComInspector = class(TPersistent)
  private
    fComObject: IDispatch;
    fFailOnError: Boolean;
    fComExceptionInfo: TExcepInfo;


    procedure InvokeHelper(Disp: IDispatch; DispId: TDispID; Flags: Word; var Params: array of Variant; var Res: OleVariant);
    function GetComMemberDisp(Disp: IDispatch; Name: PWideChar): TDispID;
    function GetFailIfComError: Boolean;
    procedure SetFailIfComError(fice: Boolean);
    Procedure RaiseComException(hr: HRESULT; ExcepInfo: PExcepInfo = nil);
    function IsLoaded: Boolean;

    function GetComObject: OleVariant;
    function GetInnerProp(Name: Unicodestring): OleVariant;
    procedure SetInnerProp(Name: Unicodestring; Value: OleVariant);
  public
    constructor Create; overload;
    constructor Create(DSP: IDispatch); overload;
    Destructor Destroy; override;

    procedure Load(DSP: IDispatch);
    // available only for iDispatchEx
    Function EnumMethodsAndProperties(l: TstringList): Boolean;

    Property Loaded: Boolean read IsLoaded;
    Property ComObject: OleVariant read GetComObject;
    Property Prop[Name: Unicodestring]: OleVariant read GetInnerProp write SetInnerProp;
    function Method(const MethodName: Unicodestring; var Params: array of Variant): OleVariant;
    Property FailIfComError: Boolean read GetFailIfComError write SetFailIfComError;
  end;

  TDispatchInvokeParams = class
  private
    WSFreeList: TList;
    procedure variantToTagVariant(const par: Variant; var ov: tagvariant);
    procedure tagVariantToVariant(const ov: tagvariant; var par: Variant);

  public
    DispParam: TDispParams;

    Constructor Create;
    Destructor Destroy; override;

    Procedure ClearUp;
    Procedure BuildFromParams(const par: Array of Variant; const PropertySet: Boolean);
    Procedure PushParamsBack(var par: array of Variant);
  end;

function IDispatchInvoke(Disp: IDispatch; PropertySet: Boolean; const MethodName: UnicodeString; var par: array of Variant; out ErrorCode: string): Variant;
function FindDispatcher(Disp: IDispatch; var Name: Unicodestring): OleVariant;

implementation

constructor TComInspector.Create;
begin
  Inherited;
  fComObject := nil;
  fFailOnError := True;

end;

function TComInspector.IsLoaded: Boolean;
begin
  Result := fComObject <> nil;
end;

function TComInspector.GetComObject: OleVariant;
begin
  Result := fComObject;
end;

function TComInspector.GetInnerProp(Name: Unicodestring): OleVariant;
var
  Dispatcher: IDispatch;
  DispId: TDispID;
  Params: array of Variant;
  ErrorCode: string;
begin
  Params := NIL;


  result := IDispatchInvoke(self.fComObject, False, Name, Params, eRRORcODE);
end;

procedure TComInspector.SetInnerProp(Name: Unicodestring; Value: OleVariant);
var
  Params: array of Variant;
  ErrorCode: string;
begin

  SetLength(Params, 1);
  Params[0] := Value;
  IDispatchInvoke(self.fComObject, True, Name, Params, ErrorCode);
  Params := NIL;
end;

function TComInspector.GetFailIfComError: Boolean;
begin
  Result := fFailOnError;
end;

procedure TComInspector.SetFailIfComError(fice: Boolean);
begin
  fFailOnError := fice;
end;

Procedure TComInspector.RaiseComException(hr: HRESULT; ExcepInfo: PExcepInfo = nil);
begin
  case hr of
    S_OK:
      raise Exception.Create('Operation succeded, but method RaiseComException has been called.');
    E_OUTOFMEMORY:
      raise Exception.Create('Out of memory.');
    DISP_E_UNKNOWNNAME:
      raise Exception.Create('One or more of the names were not known.');
    DISP_E_UNKNOWNLCID:
      raise Exception.Create('The locale identifier (LCID) was not recognized.');
    DISP_E_BADPARAMCOUNT:
      raise Exception.Create('The number of elements provided to DISPPARAMS is different from the number of arguments accepted by the method or property.');
    DISP_E_BADVARTYPE:
      raise Exception.Create('One of the arguments in rgvarg is not a valid variant type.');

    DISP_E_EXCEPTION:
      if ExcepInfo <> nil then
        raise Exception.Create(ExcepInfo.bstrDescription)
      else
        raise Exception.Create(fComExceptionInfo.bstrDescription);
    DISP_E_MEMBERNOTFOUND:
      raise Exception.Create('The requested member does not exist, or the call to Invoke tried to set the value of a read-only property.');
    DISP_E_NONAMEDARGS:
      raise Exception.Create('This implementation of IDispatch does not support named arguments.');
    DISP_E_OVERFLOW:
      raise Exception.Create('One of the arguments in rgvarg could not be coerced to the specified type.');
    DISP_E_PARAMNOTFOUND:
      raise Exception.Create('One of the parameter DISPIDs does not correspond to a parameter on the method. In this case, puArgErr should be set to the first argument that contains the error.');
    DISP_E_TYPEMISMATCH:
      raise Exception.Create('One or more of the arguments could not be coerced. The index within rgvarg of the first parameter with the incorrect type is returned in the puArgErr parameter.');
    DISP_E_UNKNOWNINTERFACE:
      raise Exception.Create('The interface identifier passed in riid is not IID_NULL.');
    DISP_E_PARAMNOTOPTIONAL:
      raise Exception.Create('A required parameter was omitted.');
    DISP_E_BADINDEX:
      raise Exception.Create('Invalid index. ');
    DISP_E_ARRAYISLOCKED:
      raise Exception.Create('Memory is locked. ');
    DISP_E_BADCALLEE:
      raise Exception.Create('Invalid callee. ');
    DISP_E_NOTACOLLECTION:
      raise Exception.Create('Does not support a collection. ');
  ELSE
    raise Exception.Create('Unknown iDispatch Error');
  end;
end;

function TComInspector.GetComMemberDisp(Disp: IDispatch; Name: PWideChar): TDispID;
var
  hr: HRESULT;
begin
  If Not Loaded then
    Raise Exception.Create('COM Container empty!');
  if Disp = nil then
    exit;
  hr := Disp.GetIDsOfNames(GUID_NULL, @Name, 1, LOCALE_SYSTEM_DEFAULT, @Result);
  if hr <> S_OK then
    If fFailOnError then
      RaiseComException(hr);
end;

procedure TComInspector.Load(DSP: IDispatch);
begin
  if DSP = nil then
    Raise Exception.Create('Attempt to load NULL interface!');
  fComObject := DSP;
end;

Function TComInspector.Method(const MethodName: Unicodestring; var Params: array of Variant): OleVariant;
var
  Dispatcher: IDispatch;
  DispathIdentifier: Integer;
  hRes: HRESULT;
  MethodResult: OleVariant;
begin
  Result := NULL;
  hRes := fComObject.GetIDsOfNames(GUID_NULL, @MethodName, 1, SysLocale.DefaultLCID, @DispathIdentifier);
  if hRes = S_OK then
  begin
    // hRes := fComObject.Invoke(DispathIdentifier, GUID_NULL, SysLocale.DefaultLCID, DISPATCH_METHOD, Params, @MethodResult, nil, nil);
    InvokeHelper(fComObject, DispathIdentifier, DISPATCH_METHOD, Params, MethodResult);
    if hRes = S_OK then
    begin
      Result := MethodResult;
    end;
  end;
end;

function FindDispatcher(Disp: IDispatch; var Name: Unicodestring): OleVariant;
var
  NextName: Unicodestring;
  DotPosition: Integer;
  DispId: TDispID;
  Params: array of Variant;
  ErrorCode: string;
begin
  DotPosition := Pos('.', Name);
  if DotPosition <= 0 then
    Result := Disp
  else
  begin
    NextName := Copy(Name, 1, DotPosition - 1);
    Params := NIL;
    Result := IDispatchInvoke(Disp, false, nextName, Params, ErrorCode);


    if not VarIsNull(Result) then
    begin
      Name := Copy(Name, DotPosition + 1, Length(Name) - 2);
      Result := FindDispatcher(Result, Name);
    end;

  end;
end;

procedure TComInspector.InvokeHelper(Disp: IDispatch; DispId: TDispID; Flags: Word; var Params: array of Variant; var Res: OleVariant);
var
  hr: HRESULT;
  aDispParams: TDispParams;
  aDispId: array of TDispID;
  aEI: TExcepInfo;
  iError: UINT;
  ParStr: Unicodestring;
  i: Integer;
  x: Integer;
  InvokeParams: TDispatchInvokeParams;
begin
    InvokeParams := TDispatchInvokeParams.Create;
  InvokeParams.BuildFromParams(Params, (Flags = DISPATCH_PROPERTYPUT) or (Flags = DISPATCH_METHOD));

  hr := Disp.Invoke(DispId, GUID_NULL, LOCALE_SYSTEM_DEFAULT, Flags, InvokeParams.DispParam, @Res, @aEI, @iError);
  InvokeParams.PushParamsBack(Params);
  InvokeParams.Free;

  if hr <> S_OK then
    If fFailOnError then
      RaiseComException(hr, @aEI)
end;

Destructor TComInspector.Destroy;
begin

  inherited;
end;

function TComInspector.EnumMethodsAndProperties(l: TstringList): Boolean;
var
  dispEx: IDispatchEx;
begin
  Result := false;
  if fComObject.QueryInterface(IDispatchEx, dispEx) = S_OK then
  begin
    Result := True;

  end;
end;

constructor TComInspector.Create(DSP: IDispatch);
begin
  Create;
  Load(DSP);
end;

var
  DispPropertyPut: Integer = DISPID_PROPERTYPUT;

function IDispatchInvoke(Disp: IDispatch; PropertySet: Boolean; const MethodName: UnicodeString; var par: array of Variant; out ErrorCode: string): Variant;
var
  self: iDispatch;
  Param: Word;
  i, ArgErr: Longint;
  DispatchId: Longint;
  InvokeParams: TDispatchInvokeParams;
  ExceptInfo: TExcepInfo;
  aName: PWideChar;
  WSFreeList: TList;
  ErrorMessage: Unicodestring;
  Name: Unicodestring;
begin
  ErrorCode:= '';
  Name := MethodName;
  self:= FindDispatcher(Disp, Name);
  if Self = nil then
  begin
    ErrorCode := ERROR_DISPATCH_IS_NULL;
    raise Exception.Create('Variant is null, cannot invoke');
  end;
  FillChar(ExceptInfo, SizeOf(ExceptInfo), 0);
  if Name = '' then
  begin
    DispatchId := 0;
  end else begin
    aName := StringToOleStr(Name);
    try
      if Self = nil then
      begin
        ErrorCode := ERROR_DISPATCH_IS_NULL;
        raise Exception.Create('S_NILInterfaceException');
      end;

      if Self.GetIDsOfNames(GUID_NULL, @aName, 1, LOCALE_SYSTEM_DEFAULT, @DispatchId) <> S_OK then
      begin
        ErrorCode := ERROR_UNKNOWN_METHODNAME;
        raise Exception.Create('UnknownMethod');
      end;



    finally
      SysFreeString(aName);
    end;
  end;
  InvokeParams := TDispatchInvokeParams.Create;

  WSFreeList := TList.Create;

  try
    InvokeParams.BuildFromParams(par, PropertySet);
    try
      if PropertySet then
        Param := DISPATCH_PROPERTYPUT
      else
        Param := DISPATCH_METHOD or DISPATCH_PROPERTYGET;

      i := Self.Invoke(DispatchId, GUID_NULL, LOCALE_SYSTEM_DEFAULT, Param, InvokeParams.DispParam, @Result, @ExceptInfo, @ArgErr);
      if Succeeded(i) then
        InvokeParams.PushParamsBack(par);

{$IFNDEF Delphi3UP}
      try
        if not Succeeded(i) then
        begin
          if i = DISP_E_EXCEPTION then
          begin
            ErrorCode := ERROR_EXTERNALEXCEPTION;
            ErrorMessage := ExceptInfo.bstrSource + ': ' + ExceptInfo.bstrDescription;
            raise Exception.Create(ErrorMessage)
          end
          else begin
          ErrorCode := ERROR_SysErrorMessage;
            raise Exception.Create(SysErrorMessage(i));
          end;
        end;
      finally
        // SysFreeString(ExceptInfo.bstrSource);
        // SysFreeString(ExceptInfo.bstrDescription);
        // SysFreeString(ExceptInfo.bstrHelpFile);
      end;
{$ELSE}
      if not Succeeded(i) then
      begin
        if i = DISP_E_EXCEPTION then
        begin
          ErrorCode := ERROR_EXTERNALEXCEPTION;
          raise Exception.Create(ExceptInfo.bstrSource + ': ' + ExceptInfo.bstrDescription)
        end else begin
        ErrorCode := ERROR_SysErrorMessage;
          raise Exception.Create(SysErrorMessage(i));
  end;
      end;
{$ENDIF}
    finally
      InvokeParams.Free;
    end;
  finally
    for i := WSFreeList.Count - 1 downto 0 do
      SysFreeString(WSFreeList[i]);
    WSFreeList.Free;
  end;
end;

{ TDispatchInvokeParams }

procedure TDispatchInvokeParams.BuildFromParams(const par: array of Variant; const PropertySet: Boolean);
var
  i: Integer;
begin
  DispParam.cNamedArgs := 0;
  DispParam.rgdispidNamedArgs := nil;
  DispParam.cArgs := (High(par) + 1);

  if PropertySet then
  begin
    DispParam.cNamedArgs := 1;
    DispParam.rgdispidNamedArgs := @DispPropertyPut;
  end;

  GetMem(DispParam.rgvarg, SizeOf(TVariantArg) * (High(par) + 1));
  FillChar(DispParam.rgvarg^, SizeOf(TVariantArg) * (High(par) + 1), 0);

  for i := 0 to High(par) do
  begin
    variantToTagVariant(par[High(par) - i], DispParam.rgvarg[i]);
  end;

end;

procedure TDispatchInvokeParams.ClearUp;
var
  i: Integer;
  p: pointer;
begin
  for i := 0 to DispParam.cArgs - 1 do
  begin
    try
      if DispParam.rgvarg[i].vt = (VT_VARIANT or VT_BYREF) then
      begin
        if {$IFDEF DELPHI4UP}POleVariant{$ELSE}PVariant{$ENDIF}
          (DispParam.rgvarg[i].pvarVal) <> nil then
          dispose(
{$IFDEF DELPHI4UP}
            POleVariant
{$ELSE}
            PVariant
{$ENDIF}
            (DispParam.rgvarg[i].pvarVal));
      end
      else
        if (DispParam.rgvarg[i].vt AND VT_BYREF) = VT_BYREF then
      begin
        if DispParam.rgvarg[i].unkval <> NIL then
          ReAllocMem(DispParam.rgvarg[i].unkval, 0);
      end;
    except
      // do nothing

    end;
  end;
  FreeMem(DispParam.rgvarg, SizeOf(TVariantArg) * (DispParam.cArgs));

  ZeroMemory(@DispParam, SizeOf(DispParam));

  for i := WSFreeList.Count - 1 downto 0 do
    SysFreeString(WSFreeList[i]);
  WSFreeList.Clear;
end;

constructor TDispatchInvokeParams.Create;
begin
  inherited;
  WSFreeList := TList.Create;
end;

destructor TDispatchInvokeParams.Destroy;
begin
  ClearUp;
  WSFreeList.Free;
  inherited;
end;

procedure TDispatchInvokeParams.PushParamsBack(var par: array of Variant);
var
  i: Integer;
begin
  for i := 0 to High(par) do
    tagVariantToVariant(DispParam.rgvarg[i], par[High(par) - i]);
end;

procedure TDispatchInvokeParams.tagVariantToVariant(const ov: tagvariant; var par: Variant);
        {$IFNDEF DELPHIXE_UP}
type
  puInt64 = ^uInt64;
{$ENDIF}

var
  pws: PWideChar;
        {$IFNDEF DELPHIXE_UP}
      aInt64: int64;
      auInt64: uInt64;
      {$ENDIF}
begin
  try
    case ov.vt of
      VT_BSTR:
        par := OleStrToString(ov.bstrVal);
      VT_BYREF or VT_BSTR:
        begin
          pws := pWideChar(ov.pbstrVal^);
          par := OleStrToString(pws);
        end;

      VT_UI1:
        par := ov.bVal; // : Byte;
      VT_I2:
        par := ov.iVal; // : Smallint;
      VT_I4:
        par := ov.lVal; // : Longint;
      VT_R4:
        par := ov.fltVal; // : Single;
      VT_R8:
        par := ov.dblVal; // : Double;
      VT_BOOL:
        par := ov.vbool; // : TOleBool;
      VT_CY:
        par := ov.cyVal; // : Currency;
      VT_DATE:
        par := ov.date; // : TOleDate;
      VT_I1:
        par := ov.cVal; // : AnsiChar;
      VT_UI2:
        par := ov.uiVal; // : Word;
      VT_UI4:
        par := ov.ulVal; // : LongWord;
      VT_I8:
        {$IFDEF DELPHIXE_UP}
        par := ov.llVal; // : Int64;
        {$ELSE}
        begin
          Move(ov.lVal, aInt64, SizeOf(aInt64));
          par := aInt64;
        end;
        {$ENDIF}
      VT_UI8:
        {$IFDEF DELPHIXE_UP}
        par := ov.ullVal; // : UInt64;
        {$ELSE}
        begin
          Move(ov.ulVal, auInt64, SizeOf(auInt64));
    par := auInt64;
        end;
        {$ENDIF}
      VT_INT:
        par := ov.intVal; // : Integer;
      VT_UINT:
        par := ov.uintVal; // : LongWord;

      VT_BYREF or VT_UI1:
        par := ov.pbVal^; // : ^Byte;
      VT_BYREF or VT_I2:
        par := ov.piVal^; // : ^Smallint;
      VT_BYREF or VT_I4:
        par := ov.plVal^; // : ^Longint;
      VT_BYREF or VT_R4:
        par := ov.pfltVal^; // : ^Single;
      VT_BYREF or VT_R8:
        par := ov.pdblVal^; // : ^Double;
      VT_BYREF or VT_BOOL:
        par := ov.pbool^; // : ^TOleBool;
      VT_BYREF or VT_CY:
        par := ov.pcyVal^; // : ^Currency;
      VT_BYREF or VT_VARIANT:
        par := ov.pvarVal^; // l: PVariant;

      VT_BYREF or VT_I1:
        par := ov.pcVal^; // l: PAnsiChar;
      VT_BYREF or VT_UI2:
        par := ov.puiVal^; // l: PWord;
      VT_BYREF or VT_UI4:
        par := ov.pulVal^; // l: PInteger;
      VT_BYREF or VT_INT:
        par := ov.pintVal^; // l: PInteger;
      VT_BYREF or VT_UINT:
        par := ov.puintVal^; // l: PLongWord;
      VT_BYREF or VT_I8:
              {$IFDEF DELPHIXE_UP}
        par := ov.pllVal^; // : ^Int64;
        {$ELSE}
                                       par := pInt64(ov.plVal)^; // : ^Int64;
        {$ENDIF}
      VT_BYREF or VT_UI8:
                      {$IFDEF DELPHIXE_UP}
        par := ov.pullVal^; // : ^UInt64;
        {$ELSE}
        par := puInt64(ov.pulVal)^; // : ^UInt64;
        {$ENDIF}
    else
      Move(ov,
        par,
        SizeOf({$IFDEF DELPHI4UP}OleVariant{$ELSE}Variant{$ENDIF}));
    end;
  except
    par := error_invalid_typecast;

  end;
end;

procedure TDispatchInvokeParams.variantToTagVariant(const par: Variant; var ov: tagvariant);
var
  nv: Variant;
  pws: PWideChar;
  pvd: pvardata;

  procedure Copy(vt: TVarType; var Buf; Size: Integer);
  var
    p: pointer;
  begin
    ov.vt := vt or VT_BYREF;
    p := NIL;
    ReAllocMem(p, Size);
    Move(Buf, pByte(p)^, Size);
    ov.unkval := p;
  end;

var
  vSmallint: Smallint;
{$IFDEF UNICODE}
  ws: string;
{$ELSE}
  ws: WideString;
{$ENDIF}
begin
  pvd := pvardata(@par);
  case pvd.VType of
  {$IFDEF UNICODE}
    varUString,
    {$ENDIF}
    varOleStr, varString:
      begin
        ov.vt := VT_BSTR or VT_BYREF;
        New(ov.pbstrVal);
        ws := par;
        pws := StringToOleStr(ws);
        ov.pbstrVal^ := pws;
        WSFreeList.add(pws);
      end;

    varSmallInt:
      Copy(VT_I2, pvd.vSmallint, SizeOf(pvd.vSmallint));
    varInteger:
      Copy(VT_I4, pvd.VInteger, SizeOf(pvd.VInteger));
    varSingle:
      Copy(VT_R4, pvd.VSingle, SizeOf(pvd.VSingle));
    varDate:
      Copy(VT_DATE, pvd.VDate, SizeOf(pvd.VDate));
    varDouble:
      Copy(VT_R8, pvd.VDouble, SizeOf(pvd.VDouble));
    varCurrency:
      Copy(VT_CY, pvd.VCurrency, SizeOf(pvd.VCurrency));

    varBoolean:
      Copy(VT_BOOL, pvd.VBoolean, SizeOf(pvd.VBoolean));
    varShortInt:
      begin
        // no noShortInt in the tag
        vSmallint := pvd.VShortInt;
        Copy(VT_I2, vSmallint, SizeOf(vSmallint));
      end;
    varByte:
      Copy(VT_UI1, pvd.VByte, SizeOf(pvd.VByte));
    varWord:
      Copy(VT_UI2, pvd.VWord, SizeOf(pvd.VWord));
    varLongWord:
      Copy(VT_UI4, pvd.VLongWord, SizeOf(pvd.VLongWord));
    varInt64:
      Copy(VT_I8, pvd.VInt64, SizeOf(pvd.VInt64));
      {$IFDEF DELPHIXE_UP}
    varUInt64:
      Copy(VT_UI8, pvd.VUInt64, SizeOf(pvd.VUInt64));
      {$ENDIF}

  else
    begin
      ov.vt := VT_VARIANT or VT_BYREF;
      New(
{$IFDEF DELPHI4UP}
        POleVariant
{$ELSE}
        PVariant{$ENDIF}
        (ov.pvarVal));

      Move(par, pointer(ov.pvarVal)^,
        SizeOf({$IFDEF DELPHI4UP}OleVariant{$ELSE}Variant{$ENDIF}));

    end;
  end;
end;


// notes
procedure EnumAnimeList;
var
	e: IEnumVariant;
	v: OleVariant;
	i: Cardinal;
begin
//	AnimeList.Clear;
//	e := Character.AnimationNames.Get_Enum as IEnumVariant;
//	while (e.Next(1, v, i) = S_OK) do
//		AnimeList.Items.Add(String(v));
//	AnimeList.Sorted := True;
end;

    end.
