Unit MaxLogic.SelfDeletingTemporaryFile;

Interface

Uses
  System.Classes;

Type
  TTempFileStream = Class(THandleStream)
  Private
    fFileName: String;
  Public
    Constructor Create(Const aFIleName: String);
    Destructor Destroy; Override;

    // FullFile name
    Property FileName: String Read fFileName;
  End;

Implementation

Uses
  System.IOUtils,
  System.SysUtils,
  Winapi.Windows;

{ TTempFileStream }

Constructor TTempFileStream.Create;
Var
  fileHandle: THandle;
  lPath: String;
Begin
  lPath := extractFilePath(aFIleName);
  If lPath = '' Then
    lPath := TPath.GetTempPath;
  lPath := IncludeTrailingPathDelimiter(lPath);

  fFileName := lPath + ExtractFileName(aFIleName);

  fileHandle := CreateFile(
    PChar(fFileName),
    GENERIC_READ Or GENERIC_WRITE,
    FILE_SHARE_READ Or FILE_SHARE_WRITE,
    Nil,
    CREATE_ALWAYS,
    FILE_ATTRIBUTE_TEMPORARY Or FILE_FLAG_DELETE_ON_CLOSE Or FILE_ATTRIBUTE_NOT_CONTENT_INDEXED,
    0);
  If fileHandle = INVALID_HANDLE_VALUE
  Then
    RaiseLastOSError(GetLastError, sLineBreak + fFileName);
  Inherited Create(fileHandle);
End;

Destructor TTempFileStream.Destroy;
Begin
  CloseHandle(Handle);
  Inherited;
End;

End.
