Unit MaxLogic.Dfm;

{ Version: 1.1
  History:
  2015-11-12: added the ability t save back the dfm file }
Interface

Uses
  windows, classes, sysUtils, RTLConsts, TypInfo, generics.collections, variants, rtti,
  generics.defaults,
  pawel1, math;

Type

{$IFNDEF UNICODE}
  UnicodeString = WideString;
{$ENDIF}
  // forward declarations
  TDFMObject = Class;
  TDFMProperty = Class;

  // allows us to acces protected methods
  TWriter = Class(System.classes.TWriter);

  TDFMProperty = Class
  Private
    fValueType: System.classes.TValueType;
    fName: String;

    fItems: TObjectList<TDFMProperty>;
    Function GetValueAsString: UnicodeString;
    Function GetItemCount: integer;
    Function GetItem(index: integer): TDFMProperty;

  Public
    Value: TValue;

    Constructor Create(aParent: TDFMProperty = Nil; aValueType: TValueType = vaNull); Overload;
    Constructor Create(aParent: TDFMProperty; aValueType: TValueType; Const aValue: TValue); Overload;

    Destructor Destroy; Override;
    Procedure Clear;
    Procedure Assign(src: TDFMProperty);
    Function IsString: Boolean;
    Function IsStringList: Boolean;

    // writes the while value using TDfmObject.WriteValue. Note, no prop name is saved here
    Procedure SaveToStream(Stream: TStream);
    Procedure SaveToByteArray(Var a: TBytes);

    Property Name: String Read fName Write fName;
    Property ValueType: TValueType Read fValueType Write fValueType;

    Property ValueAsString: String Read GetValueAsString;

    // can hold additional properties in case of: TValueType is
    // vaSet:
    // in text dfm like this
    // Propertyname = [Value1, Value2, Value3]
    // all sub properties have no name and a value of type vaIdent

    // vaCollection::
    // in the dfm file like this:
    // PropertyName = <
    // item
    // [1] // optional, just a no name property of a integer value type
    // ItemProperty1 = Item1Value1
    // endd>
    // those properties have no names, and ValueType=vaList. but have some properties theemselves, that should have some names
    // Attention, the first item property may be an orderIndex, that is a special property of type int(some) and having no name.

    // vaList: mostl likely StringList
    // in the text dfm vaList have the following format:
    // Propertyname= (
    // value1
    // value2 )

    Property ItemCount: integer Read GetItemCount;
    Property Items[Index: integer]: TDFMProperty Read GetItem;
  End;

  TDFMObject = Class
  Private
    fClassName: String;
    fName: String;
    fFlags: TFilerFlags;
    fChildPos: integer;
    fParent: TDFMObject;
    fProperties: TObjectList<TDFMProperty>;
    fObjects: TObjectList<TDFMObject>;
    fIsValid: Boolean;

    Procedure ReadObject(Reader: TReader);
    Procedure ReadHeader(Reader: TReader);
    Procedure ReadProperty(Reader: TReader; PropList: TObjectList<TDFMProperty>);
    Procedure ReadValue(Reader: TReader; Prop: TDFMProperty);

    Procedure SaveToBinary(ms: TStream);
    Procedure WriteObject(writer: TWriter);
    Procedure WriteHeader(writer: TWriter);
    Procedure WriteProperty(writer: TWriter; Prop: TDFMProperty);
    Procedure WriteValue(writer: TWriter; Prop: TDFMProperty);

    Function GetObjectCount: integer;
    Function GetDFMObject(index: integer): TDFMObject;
    Function GetPropCount: integer;
    Function GetDFMPropByName(PropName: String): TDFMProperty;
    Procedure SetFlags(Const Value: TFilerFlags);
    Procedure SetPrefixPosition(Const Value: integer);
    Procedure SetName(Const Value: String);

    Procedure ParseFromObjectBinary(Stream: TStream);
    Procedure ParseFromObjectText(Stream: TStream);
  Public
    Constructor Create;
    Destructor Destroy; Override;

    // this will open a dfm file and parse it, then output it both as a binary and as a text file. the results will be then compared to the output of the intern delphi conversion results. the files must be identicalt
    Class Procedure SelfTest(Const FileName: String);
    Class Procedure SelfTestOnDir(dir: String);

    Procedure LoadFromStream(Stream: TStream);
    Procedure LoadFromFile(Const FileName: String);
    Procedure Clear;

    Procedure SaveToStream(Stream: TStream; OutFormat: TStreamOriginalFormat = sofText);

    Procedure GetObjectList(l: TList<TDFMObject>; Recursive, IncludeThisOne: Boolean);

    // case-in-sensitive
    Function Find(Const Name: String): TDFMObject;
    // aObjectClassNamePattern may contain *
    Function GetObjectsByClass(Const aObjectClassNamePattern: String): TArray<TDFMObject>;

    Property Name: String Read fName Write SetName;
    Property ObjectClassName: String Read fClassName;
    Property Flags: TFilerFlags Read fFlags Write SetFlags;
    Property ChildPosition: integer Read fChildPos Write SetPrefixPosition;

    Property ObjectCount: integer Read GetObjectCount;
    Property Objects[Index: integer]: TDFMObject Read GetDFMObject;

    Property PropertyCount: integer Read GetPropCount;
    Property Properties: TObjectList<TDFMProperty> Read fProperties;
    Property PropByName[PropName: String]: TDFMProperty Read GetDFMPropByName;
  End;

Implementation

{ TDFMObject }

Procedure TDFMObject.ReadHeader(Reader: TReader);
Begin
  Reader.ReadPrefix(fFlags, fChildPos);
  fClassName := Reader.ReadStr;
  fName := Reader.ReadStr;
End;

Procedure TDFMObject.Clear;
Var
  x: integer;
Begin
  fObjects.Clear;
  fProperties.Clear;

  fIsValid := false;
  fClassName := '';
  fName := '';
End;

Constructor TDFMObject.Create;
Begin
  Inherited;
  fProperties := TObjectList<TDFMProperty>.Create;
  fObjects := TObjectList<TDFMObject>.Create;
End;

Destructor TDFMObject.Destroy;
Begin
  Clear;
  fProperties.Free;
  fObjects.Free;

  Inherited;
End;

Function TDFMObject.GetDFMObject(index: integer): TDFMObject;
Begin
  Result := fObjects[Index];
End;

Function TDFMObject.GetDFMPropByName(PropName: String): TDFMProperty;
Var
  s: String;
  x: integer;
Begin
  Result := Nil;
  s := lowercase(PropName);
  For x := 0 To fProperties.count - 1 Do
    If s = lowercase(Properties[x].Name) Then
    Begin
      Result := Properties[x];
      Break;
    End;
End;

Function TDFMObject.GetObjectCount: integer;
Begin
  Result := fObjects.count
End;

Function TDFMObject.GetPropCount: integer;
Begin
  Result := fProperties.count;
End;

Procedure TDFMObject.ParseFromObjectBinary(Stream: TStream);
Var
  Reader: TReader;
Begin
  Clear;

  Reader := TReader.Create(Stream, 4096);
  Try
    Reader.ReadSignature;
    ReadObject(Reader);
    fIsValid := True;
  Finally
    Reader.Free;
  End;
End;

Procedure TDFMObject.ParseFromObjectText;
Var
  ms: TMemoryStream;
Begin
  ms := TMemoryStream.Create;
  ObjectTextToBinary(Stream, ms);
  ms.Position := 0;
  ParseFromObjectBinary(ms);
  ms.Free;
End;

Procedure TDFMObject.ReadObject(Reader: TReader);
Var
  obj: TDFMObject;
Begin
  ReadHeader(Reader);

  While Not Reader.EndOfList Do
    ReadProperty(Reader, fProperties);
  Reader.ReadListEnd;

  While Not Reader.EndOfList Do
  Begin
    obj := TDFMObject.Create;
    obj.fParent := self;
    fObjects.add(obj);
    obj.ReadObject(Reader);
  End;
  Reader.ReadListEnd;
End;

Procedure TDFMObject.ReadProperty(Reader: TReader; PropList: TObjectList<TDFMProperty>);
Var
  Prop: TDFMProperty;

Begin
  Prop := TDFMProperty.Create;
  Prop.fName := Reader.ReadStr; // save for error reporting
  Prop.fValueType := Reader.NextValue;
  PropList.add(Prop);

  ReadValue(Reader, Prop);
End;

Procedure TDFMObject.ReadValue(Reader: TReader; Prop: TDFMProperty);
Var
  s: String;
  ByteCount: longint;
  CollectionItem, SubProp: TDFMProperty;
  Bytes: TBytes;
  NextValueType: TValueType;
Begin
  Prop.ValueType := Reader.NextValue;
  Prop.Value := Default (TValue);

  Case Prop.ValueType Of
    vaInt64:
      Prop.Value := Reader.ReadInt64;
    vaInt8, vaInt16, vaInt32:
      Prop.Value := Reader.ReadInteger;
    vaExtended:
      Prop.Value := Reader.ReadFloat;

    vaDouble:
      Prop.Value := Reader.ReadFloat;
    vaSingle:
      Prop.Value := TValue.From<single>(Reader.ReadSingle);
    vaCurrency:
      Prop.Value := Reader.ReadCurrency;
    vaDate:
      Prop.Value := TValue.From<TDateTime>(Reader.ReadDate);
    vaString, vaLString, vaWString, vaUTF8String:
      Prop.Value := Reader.ReadString;
    vaIdent, vaFalse, vaTrue, vaNil, vaNull:
      Prop.Value := Reader.ReadIdent;
    vaBinary:
      Begin
        Reader.ReadValue;
        Reader.Read(ByteCount, SizeOf(ByteCount));
        SetLength(Bytes, ByteCount);
        Reader.Read(Bytes, ByteCount);

        Prop.Value := TValue.From<TBytes>(Bytes);
      End;

    // in text dfm like this
    // Propertyname = [Value1, Value2, Value3]
    vaSet:
      Begin
        // swallow the value type
        Reader.ReadValue;

        // read strings, without any value type informations
        While True Do
        Begin
          s := Reader.ReadStr;
          If s = '' Then
            Break
          Else
          Begin
            TDFMProperty.Create(Prop, vaIdent, s);
          End;
        End;
      End;

    // in the dfm file like this:
    // PropertyName = <
    // item
    // [1] // optional, just a no name property of a integer value type
    // ItemProperty1 = Item1Value1
    // endd>
    vaCollection:
      Begin
        Reader.ReadValue;
        While Not Reader.EndOfList Do
        Begin
          CollectionItem := TDFMProperty.Create(Prop, vaList);
          If Reader.NextValue In [vaInt8, vaInt16, vaInt32] Then
          Begin
            SubProp := TDFMProperty.Create(CollectionItem, Reader.NextValue);
            ReadValue(Reader, SubProp);
          End;

          Reader.CheckValue(vaList);
          While Not Reader.EndOfList Do
          Begin
            ReadProperty(Reader, CollectionItem.fItems);
          End;
          Reader.ReadListEnd;

        End;
        Reader.ReadListEnd;
      End;
    // in the text dfm vaList have the following format:
    // Propertyname= (
    // value1
    // value2 )
    vaList:
      Begin
        Reader.ReadValue;
        While Not Reader.EndOfList Do
          ReadValue(Reader, TDFMProperty.Create(Prop));
        Reader.ReadListEnd;
      End;
  Else
    Raise EReadError.CreateResFmt(@sPropertyException,
      [self.fName, DotSep, Prop.Name, IntToStr(Ord(Reader.NextValue))]);
  End;
End;

Procedure TDFMObject.LoadFromStream(Stream: TStream);
Var
  sf: TStreamOriginalFormat;
Begin
  sf := TestStreamFormat(Stream);
  Case sf Of
    sofUnknown:
      Raise Exception.Create('unknown stream format');
    sofBinary:
      ParseFromObjectBinary(Stream);
    sofText,
      sofUTF8Text:
      ParseFromObjectText(Stream);
  End;
End;

Procedure TDFMObject.SetFlags(Const Value: TFilerFlags);
Begin
  fFlags := Value;
End;

Procedure TDFMObject.SetPrefixPosition(Const Value: integer);
Begin
  fChildPos := Value;
End;

Procedure TDFMObject.WriteHeader(writer: TWriter);
Begin
  writer.WritePrefix(fFlags, fChildPos);
  writer.WriteUTF8Str(fClassName);
  writer.WriteUTF8Str(fName);
End;

Procedure TDFMObject.SaveToStream(Stream: TStream;
  OutFormat: TStreamOriginalFormat);
Var
  ms, mText, mBin: TMemoryStream;
Begin
  ms := TMemoryStream.Create;
  SaveToBinary(ms);

  ms.Position := 0;
  Case OutFormat Of
    sofUnknown,
      sofBinary:
      Begin
        Stream.copyFrom(ms, ms.size);

        { mText := TMemoryStream.Create;
          ObjectBinaryToText(ms, mText);
          mText.Position := 0;
          mBin := TMemoryStream.Create;
          ObjectTextToBinary(mText, mBin);
          mBin.Position := 0;
          stream.copyFrom(mBin, mBin.size);
          mBin.Free;
          mText.Free; }
      End;
    sofUTF8Text,
      sofText:
      ObjectBinaryToText(ms, Stream, OutFormat);
  End;
  ms.Free;
End;

Procedure TDFMObject.SaveToBinary(ms: TStream);
Var
  writer: TWriter;
Begin
  writer := TWriter.Create(ms, 4096);
  Try
    writer.WriteSignature;
    WriteObject(writer);
  Finally
    writer.Free;
  End;
End;

Procedure TDFMObject.WriteObject(writer: TWriter);
Var
  Prop: TDFMProperty;
  obj: TDFMObject;
Begin
  WriteHeader(writer);

  For Prop In fProperties Do
    WriteProperty(writer, Prop);
  writer.WriteListEnd;

  For obj In fObjects Do
    obj.WriteObject(writer);
  writer.WriteListEnd;

End;

Procedure TDFMObject.WriteProperty(writer: TWriter; Prop: TDFMProperty);
Begin
  writer.WriteUTF8Str(Prop.fName);
  WriteValue(writer, Prop);
End;

Procedure TDFMObject.WriteValue(writer: TWriter; Prop: TDFMProperty);
Var
  s: String;
  ByteCount: longint;
  CollectionItem, SubProp: TDFMProperty;
  Bytes: TBytes;
  NextValueType: TValueType;
  i: integer;
  i64: int64;
  x: integer;
  ListOpen: Boolean;
Begin

  Case Prop.ValueType Of
    vaInt64:
      Begin
        i64 := Prop.Value.AsInt64;
        writer.WriteInteger(i64);
      End;
    vaInt8, vaInt16, vaInt32:
      Begin
        i := Prop.Value.AsInteger;
        writer.WriteInteger(i);
      End;
    vaExtended, vaDouble:
      writer.WriteFloat(Prop.Value.AsExtended);
    vaSingle:
      writer.WriteSingle(Prop.Value.asType<single>);
    vaCurrency:
      writer.WriteCurrency(Prop.Value.AsCurrency);
    vaDate:
      writer.WriteDate(Prop.Value.asType<TDateTime>);
    vaString, vaLString, vaWString, vaUTF8String:
      writer.WriteString(Prop.Value.AsString);
    vaIdent, vaFalse, vaTrue, vaNil, vaNull:
      writer.WriteIdent(Prop.Value.AsString);
    vaBinary:
      Begin
        writer.WriteValue(vaBinary);
        Bytes := Prop.Value.asType<TBytes>;
        ByteCount := Length(Bytes);
        writer.Write(ByteCount, SizeOf(ByteCount));
        writer.Write(Bytes, ByteCount);
      End;

    // in text dfm like this
    // Propertyname = [Value1, Value2, Value3]
    vaSet:
      Begin
        writer.WriteValue(vaSet);

        // Write strings, without any value type informations
        For SubProp In Prop.fItems Do
          writer.WriteUTF8Str(SubProp.ValueAsString);

        // the last one is empty to indicate the end of the list
        writer.WriteUTF8Str('');

      End;

    // in the dfm file like this:
    // PropertyName = <
    // item
    // [1] // optional, just a no name property of a integer value type
    // ItemProperty1 = Item1Value1
    // endd>
    vaCollection:
      Begin
        writer.WriteValue(vaCollection);
        For CollectionItem In Prop.fItems Do
        Begin
          ListOpen := false;
          For x := 0 To CollectionItem.ItemCount - 1 Do
          Begin
            SubProp := CollectionItem.Items[x];

            If (x = 0) And
              (SubProp.Name = '') And
              (SubProp.ValueType In [vaInt8, vaInt16, vaInt32])
            Then
              WriteValue(writer, SubProp)
            Else
            Begin
              If Not ListOpen Then
              Begin
                writer.WriteValue(vaList);
                ListOpen := True;
              End;
              WriteProperty(writer, SubProp);
            End;
          End;

          // in the case, the items were empty... we need to open the list to be able to lose it again
          If Not ListOpen Then
            writer.WriteValue(vaList);

          writer.WriteListEnd;
        End;
        writer.WriteListEnd;
      End;

    // in the text dfm vaList have the following format:
    // Propertyname= (
    // value1
    // value2 )
    vaList:
      Begin
        writer.WriteValue(vaList);
        For SubProp In Prop.fItems Do
          WriteValue(writer, SubProp);

        writer.WriteListEnd;
      End;
  Else
    Raise EWriteError.CreateResFmt(@sPropertyException,
      [self.fName, DotSep, Prop.Name, IntToStr(Ord(Prop.ValueType))]);
  End;
End;

Class Procedure TDFMObject.SelfTest;
Var
  OutBin, OutText, ms, InBin, InText: TMemoryStream;
  Dfm: TDFMObject;
  sf: TStreamOriginalFormat;
  l1, l2: TStringList;
  x: integer;
  FirstLineMissmatch: integer;
Begin

  ms := TMemoryStream.Create;
  InBin := TMemoryStream.Create;
  InText := TMemoryStream.Create;
  OutBin := TMemoryStream.Create;
  OutText := TMemoryStream.Create;

  ms.LoadFromFile(FileName);

  sf := sofUnknown;
  Try
    ms.Position := 0;
    sf := TestStreamFormat(ms);
    ms.Position := 0;

    // now load the file into the binary and text format as preparet by the delphi intern parsers. so we have clean data to compare to.
    Case sf Of
      sofBinary:
        Begin
          ObjectBinaryToText(ms, InText);
          InText.Position := 0;
          ObjectTextToBinary(InText, InBin);
        End;
      sofText,
        sofUTF8Text:
        Begin
          ObjectTextToBinary(ms, InBin);
          InBin.Position := 0;
          ObjectBinaryToText(InBin, InText);
        End;
    End;
  Except
    sf := sofUnknown;
  End;
  ms.Free;

  If sf <> sofUnknown Then
  Begin
    Dfm := TDFMObject.Create;
    InBin.Position := 0;
    Dfm.LoadFromStream(InBin);

    Dfm.SaveToStream(OutBin, sofBinary);

    InBin.Position := 0;
    OutBin.Position := 0;

    If (OutBin.size <> InBin.size) Or (Not CompareMem(OutBin.memory, InBin.memory, OutBin.size)) Then
      Raise Exception.Create('Bin failed on file: ' + FileName);

    Dfm.SaveToStream(OutText);

    OutText.Position := 0;
    InText.Position := 0;
    If (OutText.size <> InText.size) Or (Not CompareMem(OutText.memory, InText.memory, OutText.size)) Then
    Begin
      InText.savetofile('d:\tmp\TextIn.dfm');
      OutText.savetofile('d:\tmp\TextOut.dfm');

      // compare line by line
      l1 := TStringList.Create;
      l2 := TStringList.Create;
      InText.Position := 0;
      l1.LoadFromStream(InText);

      OutText.Position := 0;
      l2.LoadFromStream(OutText);

      FirstLineMissmatch := -1;
      For x := 0 To min(l1.count, l2.count) - 1 Do
      Begin
        If l1[x] <> l2[x] Then
        Begin
          FirstLineMissmatch := x;
          Break;
        End;
      End;

      If FirstLineMissmatch = -1 Then
        If l1.count <> l2.count Then
          FirstLineMissmatch := min(l1.count, l2.count) + 2;
      l1.Free;
      l2.Free;
      Raise Exception.Create('Text failed on line ' + IntToStr(FirstLineMissmatch + 1) + 'in file: ' + FileName);
    End;

    Dfm.Free;
  End;

  InBin.Free;
  InText.Free;
  OutBin.Free;
  OutText.Free;
End;

Class Procedure TDFMObject.SelfTestOnDir(dir: String);
Var
  files: TStringList;
  x: integer;
Begin
  beep;
  dir := IncludeTrailingPathDelimiter(dir);
  files := TStringList.Create;
  pawel1.getfilelist(files, dir, '*.dfm', True);
  For x := 0 To files.count - 1 Do
    SelfTest(dir + files[x]);

  files.Free;
End;

Procedure TDFMObject.SetName(Const Value: String);
Begin
  fName := Value;
End;

Procedure TDFMObject.LoadFromFile(Const FileName: String);
Var
  ms: TMemoryStream;
Begin
  ms := TMemoryStream.Create;
  ms.LoadFromFile(FileName);
  LoadFromStream(ms);
  ms.Free;
End;

Procedure TDFMObject.GetObjectList(l: TList<TDFMObject>; Recursive,
  IncludeThisOne: Boolean);
Var
  x: integer;
Begin
  If IncludeThisOne Then
    l.add(self);

  For x := 0 To fObjects.count - 1 Do
  Begin
    l.add(fObjects[x]);
    If Recursive Then
      fObjects[x].GetObjectList(l, True, false);
  End;
End;

Function TDFMObject.Find(Const Name: String): TDFMObject;
Var
  x: integer;
Begin
  Result := Nil;

  For x := 0 To fObjects.count - 1 Do
    If sametext(fObjects[x].Name, Name) Then
      exit(fObjects[x]);
End;

Function TDFMObject.GetObjectsByClass(Const aObjectClassNamePattern: String): TArray<TDFMObject>;
Var
  l: TList<TDFMObject>;
  x: integer;
Begin
  l := TList<TDFMObject>.Create;
  Try
    For x := 0 To ObjectCount - 1 Do
      If pawel1.StringMatches(self.Objects[x].ObjectClassName, aObjectClassNamePattern, false) Then
        l.add(Objects[x]);
    Result := l.ToArray;
  Finally
    l.Free;
  End;
End;

{ TDFMProperty }

Procedure TDFMProperty.Clear;
Begin
  fItems.Clear;
  Value := Default (TValue);
End;

Destructor TDFMProperty.Destroy;
Begin
  Clear;
  fItems.Free;
  Inherited;
End;

Function TDFMProperty.GetItem(index: integer): TDFMProperty;
Begin
  Result := fItems[Index]
End;

Function TDFMProperty.GetItemCount: integer;
Begin
  Result := fItems.count
End;

Function TDFMProperty.IsString: Boolean;
Begin
  Result := fValueType In [vaWString, vaUTF8String, vaString, vaLString]
End;

Function TDFMProperty.IsStringList: Boolean;
Begin
  Result := false;
  If fValueType = vaList Then
    If fItems.count > 0 Then
      Result := fItems[0].IsString;
End;

Function TDFMProperty.GetValueAsString: UnicodeString;
Var
  s: String;
  a: TBytes;
  fs: TFormatSettings;
  x: integer;
Begin
  Case fValueType Of
    vaList:
      Begin
        Result := '';
        For x := 0 To fItems.count - 1 Do
        Begin
          If x <> 0 Then
            Result := Result + sLineBreak;
          Result := RESULT + fItems[x].ValueAsString;
        End;
      End;
    vaSet,
      vaNil,
      vaCollection,
      vaNull:
      Result := '';

    vaInt8,
      vaInt16,
      vaInt32:
      Result := Value.AsString;

    vaInt64:
      Result := Value.AsString;

    vaUTF8String,
      vaIdent,
      vaFalse,
      vaTrue,
      vaLString,
      vaWString,
      vaString:
      Result := Value.AsString;

    vaSingle:
      Result := Value.AsString;
    vaCurrency:
      Result := Value.AsString;
    vaDate:
      Result := Value.AsString;
    vaExtended:
      Result := Value.AsString;

    vaDouble:
      Result := Value.AsString;

    vaBinary:
      Begin
        a := Value.asType<TBytes>;
        If Length(a) > 0 Then
        Begin
          SetLength(s, Length(a) * 2);
          BinToHex(a, pChar(s), Length(a));
          Result := s;
        End;

      End;
  End;

End;

Constructor TDFMProperty.Create(aParent: TDFMProperty;
  aValueType: TValueType; Const aValue: TValue);
Begin
  Create(aParent, aValueType);
  Value := aValue;
End;

Constructor TDFMProperty.Create(aParent: TDFMProperty;
  aValueType: TValueType);
Begin
  Inherited Create;
  fItems := TObjectList<TDFMProperty>.Create;

  fValueType := aValueType;
  If assigned(aParent) Then
    aParent.fItems.add(self);
End;

Procedure TDFMProperty.Assign(src: TDFMProperty);
Var
  p1, p2: TDFMProperty;
Begin
  self.fName := src.fName;
  self.Value := src.Value;
  self.fValueType := src.fValueType;

  fItems.Clear;

  For p1 In src.fItems Do
  Begin
    p2 := TDFMProperty.Create;
    fItems.add(p2);
    p2.Assign(p1);
  End;
End;

Procedure TDFMProperty.SaveToStream(Stream: TStream);
Var
  Dfm: TDFMObject;
  writer: TWriter;
Begin
  writer := TWriter.Create(Stream, 4096);
  Dfm.WriteValue(writer, self);
  Dfm.Free;
  writer.Free;
End;

Procedure TDFMProperty.SaveToByteArray(Var a: TBytes);
Var
  bs: TBytesStream;
Begin
  bs := TBytesStream.Create;
  SaveToStream(bs);
  a := bs.Bytes;
  bs.Free;
End;

Initialization


End.
