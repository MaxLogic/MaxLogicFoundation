Unit MaxLogic.SQLite;

{ Version: 2.1
  Description
  just some boiler plate code, so I do not need to create all those components each time

  NOTE: might be interesting to add pooling as described here:
  http://docwiki.embarcadero.com/CodeExamples/Tokyo/en/FireDAC.Pooling_Sample }

Interface

Uses
  sysUtils, classes, Data.DB,

  // firedac
  FireDAC.Phys.SQLiteWrapper.Stat,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.DApt, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  // FireDAC.VCLUI.Wait,
  FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteDef, FireDAC.Phys.SQLite, FireDAC.Comp.Client;

Type

  TSqlLitePack = Class
  Private
    fDbFileName: String;
    fConnection: TFDConnection;
    fDriverLink: TFDPhysSQLiteDriverLink;
    fquery: TfdQuery;
    FDSQLiteSecurity1: TFDSQLiteSecurity;
    fConnectionparams: TStringList;
    // for parameters see: http://docwiki.embarcadero.com/RADStudio/Tokyo/en/Connect_to_SQLite_database_(FireDAC)
    Procedure DoConnectionOpen;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure OpenDatabase(Const aDbFileName: String);

    Function ExecSql(Const sql: String): integer;
    Procedure open(Const sql: String);

    // remember, you have to close any open connections first
    Procedure decryptDatabase(Const DBFilename, oldPassword: String);
    Procedure EncryptDatabase(Const DBFilename, NewPassword: String);
    Function DbIsEncrypted(Const DBFilename: String): boolean;

    Property Connection: TFDConnection Read fConnection;
    Property query: TfdQuery Read fquery;

    Class Function Quote(Const value: String; Const qChar: char = ''''): String;
    Property connectionParams: TStringList Read fConnectionparams;
  End;

Implementation

Uses
{$IFDEF madExcept}madExcept, {$ENDIF}
  // vcl.clipBrd,
  autoFree;

{ TSqlLitePack }

Constructor TSqlLitePack.Create;
Begin
  Inherited Create;
  // SEE
  // http://docwiki.embarcadero.com/RADStudio/XE6/en/Connect_to_SQLite_database_(FireDAC)
  fConnectionparams := TStringList.Create;
  fConnectionparams.add('LockingMode=Normal');
  fConnectionparams.add('Synchronous=Normal');
  fConnectionparams.add('StringFormat=Unicode');
  fConnectionparams.add('Pooled=false');
  fConnectionparams.add('SharedCache=False');
  fConnectionparams.add('LockWait=True');
  fConnectionparams.add('BusyTimeout=30000');

  fDriverLink := TFDPhysSQLiteDriverLink.Create(Nil);
  // fDriverLink.EngineLinkage:= slStatic;
  fConnection := TFDConnection.Create(Nil);

  fConnection.LoginPrompt := false;
  fConnection.UpdateOptions.LockWait := True;

  fquery := TfdQuery.Create(Nil);
  fquery.Connection := fConnection;
End;

Procedure TSqlLitePack.OpenDatabase(Const aDbFileName: String);
Begin
  fDbFileName := aDbFileName;

  fConnection.Connected := false;

  fConnection.params.clear;
  fConnection.params.assign(fConnectionparams);

  fConnection.params.values['DriverID'] := 'SQLite';
  fConnection.params.values['Database'] := fDbFileName;

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
End;

Destructor TSqlLitePack.Destroy;
Begin

  fConnection.Connected := false;
  fConnectionparams.free;
  fquery.free;
  fConnection.free;
  fDriverLink.free;

  Inherited;
End;

Procedure TSqlLitePack.DoConnectionOpen;
Begin
  // hide via call stack
  // FileDac is initializing some global critical sections and does not release them..., ensure we do not cluster our leak report with them
{$IFDEF madExcept}
  HideLeak('TSqlLitePack.DoConnectionOpen');
{$ENDIF}
  fConnection.Connected := True;
End;

Function TSqlLitePack.ExecSql(Const sql: String): integer;
Begin
  fquery.sql.text := sql;
  fquery.ExecSql;
  result := fquery.RowsAffected;
End;

Procedure TSqlLitePack.open(Const sql: String);
Begin
  If fquery.Active Then
    fquery.close;

  fquery.sql.text := sql;

  fquery.open();
End;

Class Function TSqlLitePack.Quote(Const value: String; Const qChar: char = ''''): String;
Begin
  If value = '' Then
    result := qChar + qChar
  Else
  Begin
      result := value;
    If (result[1] = qChar) And (result[length(result)] = qChar) Then
    Begin
        Delete(result, 1, 1);
      Delete(result, length(result), 1);
    End;

    result := StringReplace(result, qChar, qChar + qChar, [rfReplaceAll]);
    result := qChar + result + qChar;
  End;
End;

Procedure TSqlLitePack.decryptDatabase(Const DBFilename, oldPassword: String);
Var
  FDSQLiteSecurity1: TFDSQLiteSecurity;
  garbos: TGarbos;
Begin
  FDSQLiteSecurity1 := TFDSQLiteSecurity.Create(Nil);
  gc(FDSQLiteSecurity1, garbos);
  FDSQLiteSecurity1.DriverLink := fDriverLink;

  // Use TFDSQLiteSecurity.RemovePassword to unencrypt database.
  FDSQLiteSecurity1.Database := DBFilename;
  FDSQLiteSecurity1.Password := oldPassword;
  FDSQLiteSecurity1.RemovePassword;
End;

Procedure TSqlLitePack.EncryptDatabase(Const DBFilename,
  NewPassword: String);
Var
  FDSQLiteSecurity1: TFDSQLiteSecurity;
  garbos: TGarbos;
Begin
  FDSQLiteSecurity1 := TFDSQLiteSecurity.Create(Nil);
  gc(FDSQLiteSecurity1, garbos);
  FDSQLiteSecurity1.DriverLink := fDriverLink;

  // Use TFDSQLiteSecurity.SetPassword to encrypt database. There the Password
  // property value is of <encryption algorythm>:<password> format.
  FDSQLiteSecurity1.Database := DBFilename;
  FDSQLiteSecurity1.Password := NewPassword;
  FDSQLiteSecurity1.SetPassword;
End;

Function TSqlLitePack.DbIsEncrypted(Const DBFilename: String): boolean;
Var
  State: String;
  FDSQLiteSecurity1: TFDSQLiteSecurity;
  garbos: TGarbos;
Begin
  FDSQLiteSecurity1 := TFDSQLiteSecurity.Create(Nil);
  gc(FDSQLiteSecurity1, garbos);
  FDSQLiteSecurity1.DriverLink := fDriverLink;

  // Use TFDSQLiteSecurity.CheckEncryption to request database encryption status:
  // <unencrypted> - database is unencrypted.
  // <encrypted> - database is encrypted, but the algorythm / password is wrong.
  // encryption algorythm name - database is encrypted, and the algorythm / password are correct.

  FDSQLiteSecurity1.Database := DBFilename;
  // FDSQLiteSecurity1.Password := password;
  State := FDSQLiteSecurity1.CheckEncryption;

  result := Not sameText(State, '<unencrypted>');
End;

End.
