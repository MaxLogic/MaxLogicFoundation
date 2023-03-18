Unit MaxLogic.JSON;

{ Version: 4.3

  Description:
  Small helpers for serialising and deserialising delphi objects.
  Note, now simplified as the newer delphi (tested with Berlin) has already implemented most of the required stuff on its own.
  so it is possible to have members of type TArray<T> or TObjectList<T>
  where T is a class with a parameterless constructor and they will be serialized and deserialized by delphi quite well.

}
Interface

Uses
  windows, classes, sysUtils, rest.JSON, rest.JSON.Types, rest.JsonReflect, system.JSON;

Type
  TJSONSerializable = Class
  Private
    // JSONAttribute is in REST.Json.Types
    // [JSONName('FirstName')]
    // [JSONMarshalled(false)]
  Public
    Function ToJsonString: String;virtual;
    Procedure SaveToJsonFile(Const FileName: String);

    Class Function JsonToObject<T: Class, Constructor>(Const JSON: String): T; Static;
    Class Function JsonFileToObject<T: Class, Constructor>(Const FileName: String): T; Static; static;

    Class Function clone<T: Class, Constructor>(Const aInstance: TObject): T; Static;
  End;

Function JsonObjectFromFile(Const FileName: String): TJsonObject;
Procedure JAdd(JsonObject: TJsonObject; aname: String; i: integer);

Implementation

Uses
  ioUtils;

{ TJSONSerializable }

Function TJSONSerializable.ToJsonString: String;
Begin
  result := TJson.ObjectToJsonString(self);
End;

Class Function TJSONSerializable.JsonToObject<T>(Const JSON: String): T;
Begin
  result := TJson.JsonToObject<T>(JSON);
End;

class function TJSONSerializable.clone<T>(const aInstance: TObject): T;
var
  jsonStr: String;
begin
  jsonStr:= TJson.ObjectToJsonString(aInstance);
  Result := JsonToObject<T>(jsonStr);
end;

Class
  Function TJSONSerializable.JsonFileToObject<T>(Const FileName: String): T;
Var
  s: String;
Begin
  If fileExists(FileName) Then
  Begin
      s := TFile.ReadAllText(FileName);
    result := JsonToObject<T>(s);
  End
  Else
    result := T.create;
End;

Procedure TJSONSerializable.SaveToJsonFile(Const FileName: String);
Var
  JSON: String;
Begin
  JSON := self.ToJsonString;
  TFile.WriteAllText(FileName, JSON, TEncoding.utf8);
End;

Function JsonObjectFromFile(Const FileName: String): TJsonObject;
Begin
  result :=
    TJsonObject.ParseJSONValue(
    TFile.ReadAllText(FileName)
    ) As TJsonObject;
End;

Procedure JAdd(JsonObject: TJsonObject; aname: String; i: integer);
Var
  jv: TJsonValue;
Begin
  jv := TJSONNumber.create(i);
  JsonObject.addPair(aname, jv);
End;

End.
