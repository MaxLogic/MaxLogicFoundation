unit MaxLogic.FireDacHelpers;

interface

uses
  system.classes, system.sysUtils, system.NetEncoding, Data.DB,
  FireDAC.Comp.Client;


type
  TFDQueryHelper = class helper for TFDQuery
  public
    function ExecSQLScalar: Variant; overload;
    function ExecSQLScalar(const aSql: String; const aParams: TArray<Variant>): Variant; overload;
  end;

{
  Formats: FIREDAC,BINARY,PLAIN,CSV,JSON
  plain and csv are similar, but plain is without the header, and also has a different ContextType
  Default: JSON
  Note: The Format is case-insensitive, JSON will be used if you pass some foobar here
  OutputStream will be created (as TMemoryStream) if it is passed as null }
procedure RecordsExport(const Format: String; QueryResult: TFDQuery; var OutputStream: TStream; out ContentType: String; const CsvDelimiter: Char = ',');

implementation

uses
  AutoFree, FireDAC.Stan.Intf, Variants,
  FireDAC.Comp.BatchMove.DataSet,
  FireDAC.Comp.BatchMove,
  FireDAC.Comp.BatchMove.JSON;

procedure RecordsExport(const Format: String; QueryResult: TFDQuery; var OutputStream: TStream; out ContentType: String; const CsvDelimiter: Char = ',');
var
  ContentFormat: String;
  l, row: TStringList;
  s: String;
  bm: TFDBatchMove;
  bw: TFDBatchMoveJSONWriter;
  br: TFDBatchMoveDataSetReader;
  ms: TMemoryStream;
  i: Integer;
begin
  ContentFormat := Uppercase(Trim(Format));
  ContentType := 'text/plain';
  gc(ms, TMemoryStream.Create);
  if not assigned(OutputStream) then
    OutputStream := TMemoryStream.Create;

  if (ContentFormat = 'BINARY') then
  begin
    ContentType := 'text/plain';
    QueryResult.SaveToStream(ms, sfBinary);
    ms.Position := 0;
    TNetEncoding.Base64.Encode(ms, OutputStream);
  end

  else if (ContentFormat = 'PLAIN')
    OR (ContentFormat = 'CSV') then
  begin
    gc(l, TStringList.Create);
    gc(row, TStringList.Create);
    row.Delimiter := CsvDelimiter;
    row.StrictDelimiter:= True;


    if (ContentFormat = 'PLAIN') Then
      ContentType := 'text/plain'
    else begin
      ContentType := 'text/csv';
      row.Clear;
      for i := 0 to QueryResult.FieldCount - 1 do
        row.Add(QueryResult.FieldDefs.Items[i].Name + '"');
      l.Add(row.DelimitedText);
    end;

    QueryResult.First;
    while not QueryResult.Eof do
    begin
      row.Clear;
      for i := 0 to QueryResult.FieldCount - 1 do
      begin
        VarToStrDef( QueryResult.Fields[i].AsVariant, '');
      end;
      l.Add(s);
      QueryResult.Next;
    end;
    l.SaveToStream(OutputStream);
  end



  else if ContentFormat = 'FIREDAC' then
  begin
    ContentType := 'application/json';
    OutputStream := TMemoryStream.Create;
    QueryResult.SaveToStream(OutputStream, sfJSON);
  end

  else // if ContentFormat = 'JSON' then
  begin
    ContentType := 'application/json';
    gc(bm, TFDBatchMove.Create(nil));
    gc(bw, TFDBatchMoveJSONWriter.Create(nil));
    gc(br, TFDBatchMoveDataSetReader.Create(nil));
    br.DataSet := QueryResult;
    bw.Stream := OutputStream;
    bm.Reader := br;
    bm.Writer := bw;
    bm.Execute;
  end;
end;

{ TFDQueryHelper }

function TFDQueryHelper.ExecSQLScalar: Variant;
begin
  Result:= null;
  Self.Close;
  Self.Open;
  try
    if not self.EoF then
      Result:= Fields[0].AsVariant;
  finally
    Self.Close;
  end;
end;

function TFDQueryHelper.ExecSQLScalar(const aSql: String;
  const aParams: TArray<Variant>): Variant;

  function VarTypeToFieldType(const V: Variant): TFieldType;
  begin
    case VarType(V) and varTypeMask of
      varSmallint, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64:
        Result := ftLargeint;
      varSingle, varDouble, varCurrency:
        Result := ftFloat;
      varDate:
        Result := ftDateTime;
      varBoolean:
        Result := ftBoolean;
      varString, varUString, varOleStr:
        Result := ftWideString;
    else
      Result := ftVariant;
    end;
  end;

var
  i: Integer;
  v: Variant;
begin
  Close;
  SQL.Text := aSql;

  if Length(aParams) > 0 then
  begin
    if Params.Count <> Length(aParams) then
      raise EArgumentException.CreateFmt('Parameter count mismatch. Expected %d, got %d.',
        [Params.Count, Length(aParams)]);

    for i := 0 to High(aParams) do
    begin
      v := aParams[i];
      if VarIsNull(v) or VarIsEmpty(v) then
        Params[i].Clear
      else
      begin
        Params[i].Value := v; // Variant assignment
        if Params[i].DataType = ftUnknown then
          Params[i].DataType := VarTypeToFieldType(v);
      end;
    end;
  end;

  Prepare;
  try
    Open;
    try
      Result := Fields[0].Value; // returns Variant
    finally
      Close;
    end;
  finally
    Unprepare;
  end;
end;


end.

