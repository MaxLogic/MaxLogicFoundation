Unit MaxLogic.FireDAC.Oracle;

{ Version:0221
  Description
  just some boiler plate code, so I do not need to create all those components each time

  about connection pooling:
  http://docwiki.embarcadero.com/CodeExamples/Tokyo/en/FireDAC.Pooling_Sample

  see: https://docwiki.embarcadero.com/RADStudio/Alexandria/en/Connect_to_Oracle_Server_(FireDAC)
  Windows
  To install Instant Client, download the Oracle Instant x86 or x64 client archive, unpack it and copy the files:
  oci.dll
  oraocci11.dll
  oraociei11.dll
  orasql11.dll
  oraons.dll
  in your application EXE folder or in a folder in the PATH.
  When you are using TNS names, put the tnsnames.ora file in the same folder or set the TFDPhysOracleDriverLink.TNSAdmin property value to a folder path with tnsnames.ora or use the TNSAdmin driver configuration parameter.
  Set TFDPhysOracleDriverLink.NLSLang to the required value or use the NLSLang driver configuration parameter.

  NOTE: To close all connections from a ConnectionDefinition (even the pooled ones) call:
  FDManager.CloseConnectionDef('Oracle_Pooled');
  or close the FireDAC driver manager by calling:
  FDManager.Close;


}

Interface

Uses
  sysUtils, classes, Data.DB,

  // firedac
  // FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.Phys.SQLiteDef, FireDAC.Phys.SQLite,
  FireDAC.Phys.Oracle, FireDAC.Phys.OracleCli, FireDAC.Phys.OracleWrapper, FireDAC.Phys.OracleDef,

  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.DApt, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait, FireDAC.Stan.ExprFuncs,
  FireDAC.Comp.Client;

Type

  TOracleFireDacPack = Class
  Private
    fConnection: TFDConnection;
    fquery: TfdQuery; // from FireDAC.Comp.Client
    Procedure Init;
    Class Constructor CreateClass;
    class Destructor DestroyClass;
  Public
    Class Var FormatSettings: TFormatSettings;
    class var FDPhysOracleDriverLink1: TFDPhysOracleDriverLink;
  Public
    Constructor Create(Const aConnectionDefName: String); Overload;
    // non pooled version
    Constructor Create(Const aParams: TStringList); Overload;
    Destructor Destroy; Override;

    Procedure Connect;

    Function ExecSql(Const sql: String; aClearparams: Boolean = True): integer;
    Procedure open(Const sql: String);

    Property Connection: TFDConnection Read fConnection;
    Property query: TfdQuery Read fquery;

    class Procedure CreateConnectionDefinitionDefaultParams(const aTns, aUser, aPassword, aApplicationName: String; aParams: TStringList);
    class Procedure RegisterConnectionDefinition(const aConnectionName: String; aParams: TStringList);
    class function TestConnection(const aConnectionDefinitionName: String;
      out ErrorClassName, ErrorMessage: String): Boolean;

    Class Function Quote(Const value: String; Const qChar: char = ''''): String;
    Class Function DateTimeToJulianDateStr(Const aDelphiDateTime: TDateTime): String;
    Class Function DateTimeToJulianDate(Const aDelphiDateTime: TDateTime): Double;
    // creates a commaText but with each item quoted
    Class Function JoinAndQuote(Values: TStringList): String;

    Procedure StartTransaction;
    Procedure Commit;
    Procedure Rollback;
  End;

Implementation

Uses
  {$IFDEF madExcept}madExcept, {$ENDIF}
  System.DateUtils, autoFree, ioUtils,
  MaxLogic.Logger;

{ TOracleFireDacPack }

Constructor TOracleFireDacPack.Create(Const aConnectionDefName: String);
Begin
  Inherited Create;

  Init;
  fConnection.ConnectionDefName := aConnectionDefName;
End;

Procedure TOracleFireDacPack.Commit;
Begin
  fConnection.Commit;
End;

Procedure TOracleFireDacPack.Connect;
Begin
  fConnection.Connected := false;

  // hide via call stack
  // FileDac is initializing some global critical sections and does not release them..., ensure we do not cluster our leak report with them
  {$IFDEF madExcept}
  HideLeak('TOracleFireDacPack.DoConnectionOpen');
  {$ENDIF}
  fConnection.Connected := True;
End;

Constructor TOracleFireDacPack.Create(Const aParams: TStringList);
Begin
  Inherited Create;

  Init;
  fConnection.Params.Assign(aParams);
  fConnection.Params.Values['Pooled'] := 'false';
End;

Class Constructor TOracleFireDacPack.CreateClass;
Begin
  FormatSettings := TFormatSettings.Create;
  FormatSettings.decimalSeparator := '.';

  FDPhysOracleDriverLink1 := TFDPhysOracleDriverLink.Create(nil);

  FDManager.SilentMode := True;
  // thread safe
  // FDManager is activated before threads start, by setting FDManager.Active to True.
  FDManager.Active := True
End;

class procedure TOracleFireDacPack.CreateConnectionDefinitionDefaultParams(
  const aTns, aUser, aPassword, aApplicationName: String; aParams: TStringList);

  procedure w(const aName, aValue: String);
  begin
    aParams.Values[trim(aName)] := trim(aValue);
  end;

begin
  w('DriverID', 'Ora');
  w('Database', aTns);
  w('User_Name', aUser);
  w('Password', aPassword);
  w('CharacterSet', 'UTF8');
  w('ApplicationName', aApplicationName);
  w('Pooled', 'true');
  w('POOL_MaximumItems', '30');
  w('POOL_ExpireTimeout', '30000'); // 5 minutes
end;

Class Function TOracleFireDacPack.DateTimeToJulianDate(Const aDelphiDateTime: TDateTime): Double;
Begin
  Result := System.DateUtils.DateTimeToJulianDate(aDelphiDateTime);
End;

Class Function TOracleFireDacPack.DateTimeToJulianDateStr(Const aDelphiDateTime: TDateTime): String;
Var
  f: Double;
Begin
  f := TOracleFireDacPack.DateTimeToJulianDate(aDelphiDateTime);
  Result := FloatToStr(f, FormatSettings);
End;

Destructor TOracleFireDacPack.Destroy;
Begin

  fConnection.Connected := false;
  fquery.free;
  fConnection.free;

  Inherited;
End;

class destructor TOracleFireDacPack.DestroyClass;
begin
  FDManager.Close;
  FDPhysOracleDriverLink1.free;
  FDPhysOracleDriverLink1 := nil;
end;

Function TOracleFireDacPack.ExecSql(Const sql: String; aClearparams: Boolean = True): integer;
Begin
  fquery.Close;
  If aClearparams Then
    fquery.Params.Clear;

  fquery.sql.text := sql;
  fquery.ExecSql;
  Result := fquery.RowsAffected;
End;

Procedure TOracleFireDacPack.Init;
Begin
  fConnection := TFDConnection.Create(Nil);
  fConnection.LoginPrompt := false;
  fConnection.UpdateOptions.LockWait := True;

  fquery := TfdQuery.Create(Nil);
  fquery.Connection := fConnection;
  fquery.FetchOptions.UniDirectional := True;
End;

class function TOracleFireDacPack.JoinAndQuote(Values: TStringList): String;
var
  x: integer;
begin
  Result := '';
  for x := 0 to Values.Count - 1 do
  begin
    if x = 0 then
      Result := Quote(Values[x])
    else
      Result := Result + ',' + Quote(Values[x]);
  end;
end;

Procedure TOracleFireDacPack.open(Const sql: String);
Begin
  If fquery.Active Then
    fquery.Close;

  fquery.sql.text := sql;

  fquery.open();
End;

Class Function TOracleFireDacPack.Quote(Const value: String; Const qChar: char = ''''): String;
Begin
  If value = '' Then
    Result := qChar + qChar
  Else
  Begin
    Result := value;
    If (Result[1] = qChar) And (Result[length(Result)] = qChar) Then
    Begin
      Delete(Result, 1, 1);
      Delete(Result, length(Result), 1);
    End;

    Result := StringReplace(Result, qChar, qChar + qChar, [rfReplaceAll]);
    Result := qChar + Result + qChar;
  End;
End;

class procedure TOracleFireDacPack.RegisterConnectionDefinition(
  const aConnectionName: String; aParams: TStringList);
begin
  FDManager.AddConnectionDef(aConnectionName, 'Ora', aParams);
end;

Procedure TOracleFireDacPack.Rollback;
Begin
  fConnection.Rollback;
End;

Procedure TOracleFireDacPack.StartTransaction;
Begin
  fConnection.StartTransaction;
End;

class function TOracleFireDacPack.TestConnection(
  const aConnectionDefinitionName: String;
  out ErrorClassName, ErrorMessage: String): Boolean;
var
  ora: TOracleFireDacPack;
begin
  Result := false;
  ora:= nil;
  try
    ora:= TOracleFireDacPack.Create(aConnectionDefinitionName);
    ora.Connect;
    FreeAndNil(ora);
    Result := True;
  Except
    on e:Exception do
    begin
      ErrorClassName:= e.ClassName;
      errorMessage:= e.Message;

      FreeAndNil(ora);
      Result := false;
      // if the pool is active, then the connection just got put into the pool...
      FDManager.CloseConnectionDef(aConnectionDefinitionName);
    end;
  end;
end;

End.
