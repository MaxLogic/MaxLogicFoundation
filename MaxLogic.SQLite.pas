unit maxLogic.SQLite;

{ Version: 2.1
  Description
  just some boiler plate code, so I do not need to create all those components each time

  NOTE: might be interesting to add pooling as described here:
  http://docwiki.embarcadero.com/CodeExamples/Tokyo/en/FireDAC.Pooling_Sample }

interface

uses
  SysUtils, Classes, Data.DB,

  // firedac
  FireDAC.Phys.SQLiteWrapper.Stat,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.DApt, FireDAC.Stan.def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  // FireDAC.VCLUI.Wait,
  FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteDef, FireDAC.Phys.SQLite, FireDAC.Comp.Client,
  AutoFree;

type

  TSqlLitePack = class
  private
    fDbFileName: string;
    fConnection: TFDConnection;
    fDriverLink: TFDPhysSQLiteDriverLink;
    fquery: TfdQuery;
    FDSQLiteSecurity1: TFDSQLiteSecurity;
    fConnectionparams: TStringList;
    // for parameters see: http://docwiki.embarcadero.com/RADStudio/Tokyo/en/Connect_to_SQLite_database_(FireDAC)
    procedure DoConnectionOpen;
  public
    constructor Create;
    destructor Destroy; override;

    procedure OpenDatabase(const aDbFileName: string);

    function ExecSQL(const sql: string): integer;
    procedure Open(const sql: string);

    // remember, you have to close any open connections first
    procedure DecryptDatabase(const DBFilename, oldPassword: string);
    procedure EncryptDatabase(const DBFilename, NewPassword: string);
    function DbIsEncrypted(const DBFilename: string): boolean;
    // creates a fresh independant query instance. REMARK: you need to free this query
    function CreateQuery: TFDQuery; overload;
    // as the above, but uses autoFree.pas to handle the destruction of that query automatically
    function CreateQuery(out Q: TFDQuery): iGarbo; overload;

    property Connection: TFDConnection read fConnection;
    property Query: TfdQuery read fquery;

    class function Quote(const Value: string; const qChar: char = ''''): string;
    property connectionParams: TStringList read fConnectionparams;
  end;

implementation

uses
  {$IFDEF madExcept}madExcept,{$ENDIF}
  system.ioUtils, system.strUtils;

{ TSqlLitePack }

constructor TSqlLitePack.Create;
begin
  inherited Create;
  // SEE
  // http://docwiki.embarcadero.com/RADStudio/XE6/en/Connect_to_SQLite_database_(FireDAC)
  fConnectionparams := TStringList.Create;
  fConnectionparams.Add('LockingMode=Normal');
  fConnectionparams.Add('Synchronous=Normal');
  fConnectionparams.Add('StringFormat=Unicode');
  fConnectionparams.Add('Pooled=false');
  fConnectionparams.Add('SharedCache=False');
  fConnectionparams.Add('LockWait=True');
  fConnectionparams.Add('BusyTimeout=30000');

  fDriverLink := TFDPhysSQLiteDriverLink.Create(nil);
  // fDriverLink.EngineLinkage:= slStatic;
  fConnection := TFDConnection.Create(nil);

  fConnection.LoginPrompt := False;
  fConnection.UpdateOptions.LockWait := True;
  fConnection.ResourceOptions.MacroExpand := False;

  fquery := TfdQuery.Create(nil);
  fquery.Connection := fConnection;
end;

procedure TSqlLitePack.OpenDatabase(const aDbFileName: string);
begin
  fDbFileName := aDbFileName;

  fConnection.Connected := False;

  fConnection.params.Clear;
  fConnection.params.Assign(fConnectionparams);

  fConnection.params.Values['DriverID'] := 'SQLite';
  fConnection.params.Values['Database'] := fDbFileName;

  // fConnection.params.add()('NewPassword=');

  DoConnectionOpen;

  // set some params
  // see:
  // https://www.sqlite.org/pragma.html#pragma_auto_vacuum
  // https://www.sqlite.org/pragma.html
  // ExecSql('PRAGMA journal_mode = WAL ;');
  // Auto-vacuuming is only possible if the database stores some additional information that allows each database page to be traced backwards to its referrer.
  // Therefore, auto-vacuuming must be turned on before any tables are created. It is not possible to enable or disable auto-vacuum after a table has been created.
  // ExecSql('PRAGMA auto_vacuum = 1;');
end;

destructor TSqlLitePack.Destroy;
begin

  fConnection.Connected := False;
  fConnectionparams.Free;
  fquery.Free;
  fConnection.Free;
  fDriverLink.Free;

  inherited;
end;

procedure TSqlLitePack.DoConnectionOpen;
begin
  // hide via call stack
  // FileDac is initializing some global critical sections and does not release them..., ensure we do not cluster our leak report with them
  {$IFDEF madExcept}
  HideLeak('TSqlLitePack.DoConnectionOpen');
  {$ENDIF}
  fConnection.Connected := True;
end;

function TSqlLitePack.ExecSQL(const sql: string): integer;
begin
  fquery.sql.Text := sql;
  fquery.ExecSQL;
  Result := fquery.RowsAffected;
end;

procedure TSqlLitePack.Open(const sql: string);
begin
  if fquery.Active then
    fquery.Close;

  fquery.sql.Text := sql;

  fquery.Open();
end;

class function TSqlLitePack.Quote(const Value: string; const qChar: char = ''''): string;
begin
  if Value = '' then
    Result := qChar + qChar
  else
  begin
    Result := Value;
    if (Result[1] = qChar) and (Result[length(Result)] = qChar) then
    begin
      delete(Result, 1, 1);
      delete(Result, length(Result), 1);
    end;

    Result := Stringreplace(Result, qChar, qChar + qChar, [rfReplaceAll]);
    Result := qChar + Result + qChar;
  end;
end;

procedure TSqlLitePack.decryptDatabase(const DBFilename, oldPassword: string);
var
  FDSQLiteSecurity1: TFDSQLiteSecurity;
  garbos: TGarbos;
begin
  FDSQLiteSecurity1 := TFDSQLiteSecurity.Create(nil);
  gc(FDSQLiteSecurity1, garbos);
  FDSQLiteSecurity1.DriverLink := fDriverLink;

  // Use TFDSQLiteSecurity.RemovePassword to unencrypt database.
  FDSQLiteSecurity1.Database := DBFilename;
  FDSQLiteSecurity1.Password := oldPassword;
  FDSQLiteSecurity1.RemovePassword;
end;

procedure TSqlLitePack.EncryptDatabase(const DBFilename,
  NewPassword: string);
var
  FDSQLiteSecurity1: TFDSQLiteSecurity;
  garbos: TGarbos;
begin
  FDSQLiteSecurity1 := TFDSQLiteSecurity.Create(nil);
  gc(FDSQLiteSecurity1, garbos);
  FDSQLiteSecurity1.DriverLink := fDriverLink;

  // Use TFDSQLiteSecurity.SetPassword to encrypt database. There the Password
  // property value is of <encryption algorythm>:<password> format.
  FDSQLiteSecurity1.Database := DBFilename;
  FDSQLiteSecurity1.Password := NewPassword;
  FDSQLiteSecurity1.SetPassword;
end;

function TSqlLitePack.CreateQuery(out Q: TFDQuery): iGarbo;
begin
  Q:= CreateQuery;
  Result:= gc(Q);
end;

function TSqlLitePack.CreateQuery: TFDQuery;
begin
  Result:= TfdQuery.Create(nil);
  Result.Connection := fConnection;
end;

function TSqlLitePack.DbIsEncrypted(const DBFilename: string): boolean;
var
  State: string;
  FDSQLiteSecurity1: TFDSQLiteSecurity;
  garbos: TGarbos;
begin
  FDSQLiteSecurity1 := TFDSQLiteSecurity.Create(nil);
  gc(FDSQLiteSecurity1, garbos);
  FDSQLiteSecurity1.DriverLink := fDriverLink;

  // Use TFDSQLiteSecurity.CheckEncryption to request database encryption status:
  // <unencrypted> - database is unencrypted.
  // <encrypted> - database is encrypted, but the algorythm / password is wrong.
  // encryption algorythm name - database is encrypted, and the algorythm / password are correct.

  FDSQLiteSecurity1.Database := DBFilename;
  // FDSQLiteSecurity1.Password := password;
  State := FDSQLiteSecurity1.CheckEncryption;

  Result := not sameText(State, '<unencrypted>');
end;

end.

