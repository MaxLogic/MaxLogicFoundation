unit MaxLogic.TempFileHolder;
{
  deletes temporary files when it goes out of scope
}

interface

uses
  system.SysUtils, system.Classes;

type
  ITempFileHolder = interface
    ['{BFEC285A-F58B-46AC-BEB3-2827BD6E8BCD}']

    function GetDirectoryList: TStringList;
    function GetFileList: TStringList;

    /// <summary>
    /// stores the filename to the list of files that will be deleted when this class goes out of scope
    /// </summary>
    procedure Add(const aFileName: String);

    /// <summary>
    /// Adds a directory path to the list of directories that will be deleted when this instance gets out of scope
    /// </summary>
    /// <remarks>
    /// NOTE: it will delete this directory together with all its sub items
    /// </remarks>
    procedure AddDirectory(const aDirectoryPath: String);

    // access to the underyling lists
    property FileList: TStringList read GetFileList;
    property DirectoryList: TStringList read GetDirectoryList;
  end;

  TTempFileHolder = class(TInterfacedObject, ITempFileHolder)
  private
    fFiles: TStringList;
    fDirectories: TStringList;
    function GetDirectoryList: TStringList;
    function GetFileList: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// stores the filename to the list of files that will be deleted when this class goes out of scope
    /// </summary>
    procedure Add(const aFileName: String);
    /// <summary>
    /// Adds a directory path to the list of directories that will be deleted when this instance gets out of scope
    /// </summary>
    /// <remarks>
    /// NOTE: it will delete this directory together with all its sub items
    /// </remarks>
    procedure AddDirectory(const aDirectoryPath: String);

    // access to the underyling lists
    property FileList: TStringList read GetFileList;
    property DirectoryList: TStringList read GetDirectoryList;
  end;

implementation

uses
  system.IOUtils;

{ TTempFileHolder }

procedure TTempFileHolder.Add(const aFileName: String);
begin
  fFiles.Add(aFileName);
end;

procedure TTempFileHolder.AddDirectory(const aDirectoryPath: String);
begin
  fDirectories.Add(aDirectoryPath)
end;

constructor TTempFileHolder.Create;
begin
  inherited Create;
  fFiles := TStringList.Create;
  fDirectories := TStringList.Create;
end;

destructor TTempFileHolder.Destroy;
begin
  for var lFileName in fFiles do
  begin
    if TFile.Exists(lFileName) then
      DeleteFile(lFileName);
  end;
  fFiles.Free;

  for var lDir in fDirectories do
  begin
    if TDirectory.Exists(lDir) then
      TDirectory.Delete(lDir, true);
  end;
  fDirectories.Free;

  inherited;
end;

function TTempFileHolder.GetDirectoryList: TStringList;
begin
  Result := fDirectories;
end;

function TTempFileHolder.GetFileList: TStringList;
begin
  Result := fFiles;
end;

end.
