unit MaxLogic.KnownFolders;

interface

uses
  windows, classes, sysUtils, generics.collections,
  Winapi.ShlObj, ioUtils, ComObj, ActiveX;

type
  // forward declaration
  TKnownFolders = class;

  TKnownFolderInfo = record
    Id: TIID;
    category: TKFCategory;
    Path: string;
    Name: string;
    Description: string;
    LocalizedName: String;
    Function IdAsString: string;
  end;

  TKnownFolders = class
  public
    // FOR A LIST OF KNOWN FOLDER IDS SEE:
    // https://msdn.microsoft.com/en-us/library/dd378457%28v=vs.85%29.aspx
    class function Getpath(const Id: TIID; const Default: string): string;

    class function GetDownloadsPath: string;
    class function EnumerateKnownFolders: TArray<TKnownFolderInfo>;

  end;

implementation

uses
  pawel1;

{ TKnownFolders }

class function TKnownFolders.EnumerateKnownFolders: TArray<TKnownFolderInfo>;
var
  KnownFolderManager: IKnownFolderManager;
  f: iKnownFolder;
  hr: hResult;

  KFId: array of TKnownFolderID;
  Count: UINT;
  x: Integer;
  a: TArray<TKnownFolderInfo>;
  df: TKnownFolderDefinition;
  i: Integer;
  pp: LPWSTR;
begin
  result := nil;

  hr := CoCreateInstance(CLSID_KnownFolderManager, nil, CLSCTX_ALL, IKnownFolderManager, KnownFolderManager);
  if (hr <> S_OK) or (KnownFolderManager = nil) then
    exit;

  // When this method returns, contains a pointer to an array of all KNOWNFOLDERID values registered with the system. Use CoTaskMemFree to free these resources when they are no longer needed.
  hr := KnownFolderManager.GetFolderIds(KFId, Count);
  if hr <> S_OK then
    exit;

  SetLength(a, Count);
  i := 0;
  for x := 0 to Count - 1 do
  begin
    hr := KnownFolderManager.getfolder(KFId[x], f);
    if hr <> S_OK then
      continue;

    hr := f.GetFolderDefinition(df);
    if hr = S_OK then
    begin

      // When this method returns, contains the address of a pointer to a null-terminated buffer that contains the path. The calling application is responsible for calling CoTaskMemFree to free this resource when it is no longer needed.
      hr := f.Getpath(0, pp);
      if hr = S_OK then
      begin

        a[i].Path := pp;
        a[i].Id := KFId[x];
        a[x].category := df.category;
        a[x].Name := df.pszName;
        a[x].Description := df.pszDescription;
        a[x].LocalizedName := df.pszLocalizedName;

        Inc(i);
        CoTaskMemFree(pp);
      end;

      FreeKnownFolderDefinitionFields(df);
    end;
  end;
  SetLength(a, i);
  result := a;

  CoTaskMemFree(KFId);
end;

class
  function TKnownFolders.GetDownloadsPath: string;
var
  Default: string;
  FOLDERID_Downloads: TGUID;
begin
  default := pawel1.GetSpecialFolderPath(sfkCurUser_My_Documents);

  FOLDERID_Downloads := StringToGuid('{374DE290-123F-4565-9164-39C4925E467B}');

  result := Getpath(
    FOLDERID_Downloads,
    default);

  result := IncludeTrailingPathDelimiter(result);
end;

class
  function TKnownFolders.Getpath(const Id: TIID; const Default: string): string;
var
  KnownFolderManager: IKnownFolderManager;
  f: iKnownFolder;
  hr: hResult;
  pp: LPWSTR;
begin
  result := default;

  hr := CoCreateInstance(CLSID_KnownFolderManager, nil, CLSCTX_ALL, IKnownFolderManager, KnownFolderManager);
  if (hr <> S_OK) or (KnownFolderManager = nil) then
    exit;

  hr := KnownFolderManager.getfolder(Id, f);
  if (hr <> S_OK) or (f = nil) then
    exit;

  hr := f.Getpath(0, pp);
  if hr = S_OK then
  begin
    result := pp;
    CoTaskMemFree(pp);
  end;
end;

{ TKnownFolderInfo }

function TKnownFolderInfo.IdAsString: string;
begin
  // You can use StringFromCLSID or StringFromGUID2 to convert the retrieved KNOWNFOLDERID values to strings.
  result := GuidToString(Id);
end;

end.
