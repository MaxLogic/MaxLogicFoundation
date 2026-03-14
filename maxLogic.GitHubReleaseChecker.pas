unit MaxLogic.GitHubReleaseChecker;

interface

uses
  System.JSON,
  System.Net.HttpClient,
  System.SysUtils;

type
  TGitHubReleaseCheckStatus = (
    gcsSuccess,
    gcsNoRelease,
    gcsHttpError,
    gcsInvalidResponse,
    gcsRequestFailed
  );

  TGitHubReleaseAsset = record
    Name: string;
    BrowserDownloadUrl: string;
    ContentType: string;
    Size: Int64;
    UpdatedAtUtc: TDateTime;
  end;

  TGitHubReleaseInfo = record
    TagName: string;
    Name: string;
    HtmlUrl: string;
    PublishedAtUtc: TDateTime;
    Draft: Boolean;
    Prerelease: Boolean;
    Assets: TArray<TGitHubReleaseAsset>;
    function DisplayName: string;
  end;

  TGitHubReleaseCheckResult = record
    Status: TGitHubReleaseCheckStatus;
    CurrentVersion: string;
    LatestRelease: TGitHubReleaseInfo;
    HttpStatusCode: Integer;
    HttpStatusText: string;
    ErrorMessage: string;
    function HasRelease: Boolean;
    function IsUpdateAvailable: Boolean;
  end;

  TMaxGitHubReleaseChecker = class
  strict private
    fApiVersion: string;
    fOwner: string;
    fRepository: string;
    fTimeoutMs: Integer;
    function ExecuteLatestReleaseRequest(out aResponseBody: string; out aStatusCode: Integer; out aStatusText: string): Boolean;
    class function CompareVersionSuffixes(const aLeftSuffix, aRightSuffix: string): Integer; static;
    class function NormalizeVersionTag(const aTag: string): string; static;
    class function TryParseAsset(const aAssetObject: TJSONObject; out aAsset: TGitHubReleaseAsset;
      out aErrorMessage: string): Boolean; static;
    class function TryParseIsoDateTime(const aValue: string; out aDateTime: TDateTime; out aErrorMessage: string): Boolean; static;
    class function TrySplitVersion(const aVersion: string; out aCoreParts: TArray<Int64>; out aSuffix: string): Boolean; static;
  public
    constructor Create(const aOwner, aRepository: string);
    function CheckLatestRelease(const aCurrentVersion: string = ''): TGitHubReleaseCheckResult;
    function LatestReleaseUrl: string;
    class function CompareVersionTags(const aLeft, aRight: string): Integer; static;
    class function TryParseLatestReleaseJson(const aJson: string; out aRelease: TGitHubReleaseInfo;
      out aErrorMessage: string): Boolean; static;
    property ApiVersion: string read fApiVersion write fApiVersion;
    property TimeoutMs: Integer read fTimeoutMs write fTimeoutMs;
  end;

implementation

uses
  System.DateUtils,
  System.Math,
  System.StrUtils;

const
  cAcceptHeader = 'application/vnd.github+json';
  cDefaultApiVersion = '2026-03-10';
  cDefaultTimeoutMs = 10000;
  cGitHubApiBaseUrl = 'https://api.github.com/repos/';
  cLatestReleasePath = '/releases/latest';
  cUserAgent = 'MaxLogic.GitHubReleaseChecker/1.0';

constructor TMaxGitHubReleaseChecker.Create(const aOwner, aRepository: string);
begin
  inherited Create;
  fOwner := Trim(aOwner);
  fRepository := Trim(aRepository);
  fApiVersion := cDefaultApiVersion;
  fTimeoutMs := cDefaultTimeoutMs;
end;

function TMaxGitHubReleaseChecker.CheckLatestRelease(const aCurrentVersion: string): TGitHubReleaseCheckResult;
var
  lErrorMessage: string;
  lResponseBody: string;
begin
  Result := Default(TGitHubReleaseCheckResult);
  Result.CurrentVersion := Trim(aCurrentVersion);

  try
    if not ExecuteLatestReleaseRequest(lResponseBody, Result.HttpStatusCode, Result.HttpStatusText) then
    begin
      Result.Status := TGitHubReleaseCheckStatus.gcsRequestFailed;
      Result.ErrorMessage := 'GitHub request failed before a response was received.';
      Exit;
    end;

    case Result.HttpStatusCode of
      200:
        begin
          if TryParseLatestReleaseJson(lResponseBody, Result.LatestRelease, lErrorMessage) then
            Result.Status := TGitHubReleaseCheckStatus.gcsSuccess
          else
          begin
            Result.Status := TGitHubReleaseCheckStatus.gcsInvalidResponse;
            Result.ErrorMessage := lErrorMessage;
          end;
        end;
      404:
        begin
          Result.Status := TGitHubReleaseCheckStatus.gcsNoRelease;
          Result.ErrorMessage := 'No published GitHub release found.';
        end;
    else
      begin
        Result.Status := TGitHubReleaseCheckStatus.gcsHttpError;
        Result.ErrorMessage := Format('GitHub API error: %d %s', [Result.HttpStatusCode, Result.HttpStatusText]);
      end;
    end;
  except
    on lException: Exception do
    begin
      Result.Status := TGitHubReleaseCheckStatus.gcsRequestFailed;
      Result.ErrorMessage := lException.ClassName + ': ' + lException.Message;
    end;
  end;
end;

class function TMaxGitHubReleaseChecker.CompareVersionSuffixes(const aLeftSuffix, aRightSuffix: string): Integer;
var
  lIndex: Integer;
  lLeftIsNumeric: Boolean;
  lLeftPart: string;
  lLeftParts: TArray<string>;
  lLeftValue: Int64;
  lRightIsNumeric: Boolean;
  lRightPart: string;
  lRightParts: TArray<string>;
  lRightValue: Int64;
  lSeparatorChars: TSysCharSet;
begin
  lLeftPart := TrimLeft(aLeftSuffix);
  lRightPart := TrimLeft(aRightSuffix);

  lSeparatorChars := ['-', '.', '_', '+'];
  while (lLeftPart <> '') and CharInSet(lLeftPart[1], lSeparatorChars) do
    Delete(lLeftPart, 1, 1);
  while (lRightPart <> '') and CharInSet(lRightPart[1], lSeparatorChars) do
    Delete(lRightPart, 1, 1);

  if (lLeftPart = '') and (lRightPart = '') then
    Exit(0);
  if lLeftPart = '' then
    Exit(1);
  if lRightPart = '' then
    Exit(-1);

  lLeftParts := SplitString(lLeftPart, '.-_+');
  lRightParts := SplitString(lRightPart, '.-_+');

  for lIndex := 0 to Min(High(lLeftParts), High(lRightParts)) do
  begin
    lLeftIsNumeric := TryStrToInt64(lLeftParts[lIndex], lLeftValue);
    lRightIsNumeric := TryStrToInt64(lRightParts[lIndex], lRightValue);

    if lLeftIsNumeric and lRightIsNumeric then
    begin
      if lLeftValue > lRightValue then
        Exit(1);
      if lLeftValue < lRightValue then
        Exit(-1);
    end else
    if lLeftIsNumeric <> lRightIsNumeric then
    begin
      if lLeftIsNumeric then
        Exit(-1)
      else
        Exit(1);
    end else
    begin
      Result := CompareText(lLeftParts[lIndex], lRightParts[lIndex]);
      if Result <> 0 then
        Exit;
    end;
  end;

  if Length(lLeftParts) > Length(lRightParts) then
    Exit(1);
  if Length(lLeftParts) < Length(lRightParts) then
    Exit(-1);
  Result := 0;
end;

class function TMaxGitHubReleaseChecker.CompareVersionTags(const aLeft, aRight: string): Integer;
var
  lLeftValue: Int64;
  lIndex: Integer;
  lLeftCoreParts: TArray<Int64>;
  lLeftSuffix: string;
  lRightValue: Int64;
  lRightCoreParts: TArray<Int64>;
  lRightSuffix: string;
begin
  if SameText(Trim(aLeft), Trim(aRight)) then
    Exit(0);

  if TrySplitVersion(aLeft, lLeftCoreParts, lLeftSuffix) and TrySplitVersion(aRight, lRightCoreParts, lRightSuffix) then
  begin
    for lIndex := 0 to Max(Length(lLeftCoreParts), Length(lRightCoreParts)) - 1 do
    begin
      if lIndex < Length(lLeftCoreParts) then
        lLeftValue := lLeftCoreParts[lIndex]
      else
        lLeftValue := 0;
      if lIndex < Length(lRightCoreParts) then
        lRightValue := lRightCoreParts[lIndex]
      else
        lRightValue := 0;

      if lLeftValue > lRightValue then
        Exit(1);
      if lLeftValue < lRightValue then
        Exit(-1);
    end;

    Exit(CompareVersionSuffixes(lLeftSuffix, lRightSuffix));
  end;

  Result := CompareText(NormalizeVersionTag(aLeft), NormalizeVersionTag(aRight));
end;

function TMaxGitHubReleaseChecker.ExecuteLatestReleaseRequest(out aResponseBody: string; out aStatusCode: Integer;
  out aStatusText: string): Boolean;
var
  lHttp: THTTPClient;
  lResponse: IHTTPResponse;
begin
  aResponseBody := '';
  aStatusCode := 0;
  aStatusText := '';

  lHttp := THTTPClient.Create;
  try
    lHttp.ConnectionTimeout := fTimeoutMs;
    lHttp.ResponseTimeout := fTimeoutMs;
    lHttp.HandleRedirects := True;
    lHttp.CustomHeaders['Accept'] := cAcceptHeader;
    lHttp.CustomHeaders['User-Agent'] := cUserAgent;
    lHttp.CustomHeaders['X-GitHub-Api-Version'] := fApiVersion;

    lResponse := lHttp.Get(LatestReleaseUrl);
    Result := Assigned(lResponse);
    if Result then
    begin
      aStatusCode := lResponse.StatusCode;
      aStatusText := lResponse.StatusText;
      aResponseBody := lResponse.ContentAsString(TEncoding.UTF8);
    end;
  finally
    lHttp.Free;
  end;
end;

function TMaxGitHubReleaseChecker.LatestReleaseUrl: string;
begin
  Result := cGitHubApiBaseUrl + fOwner + '/' + fRepository + cLatestReleasePath;
end;

class function TMaxGitHubReleaseChecker.NormalizeVersionTag(const aTag: string): string;
begin
  Result := Trim(aTag);
  if (Length(Result) > 1) and CharInSet(Result[1], ['v', 'V']) and CharInSet(Result[2], ['0'..'9']) then
    Delete(Result, 1, 1);
end;

class function TMaxGitHubReleaseChecker.TryParseAsset(const aAssetObject: TJSONObject; out aAsset: TGitHubReleaseAsset;
  out aErrorMessage: string): Boolean;
var
  lUpdatedAt: string;
begin
  aAsset := Default(TGitHubReleaseAsset);
  aErrorMessage := '';

  if not Assigned(aAssetObject) then
  begin
    aErrorMessage := 'GitHub release asset entry is missing.';
    Exit(False);
  end;

  aAsset.Name := aAssetObject.GetValue<string>('name', '');
  aAsset.BrowserDownloadUrl := aAssetObject.GetValue<string>('browser_download_url', '');
  aAsset.ContentType := aAssetObject.GetValue<string>('content_type', '');
  aAsset.Size := aAssetObject.GetValue<Int64>('size', 0);

  lUpdatedAt := aAssetObject.GetValue<string>('updated_at', '');
  if lUpdatedAt <> '' then
    Result := TryParseIsoDateTime(lUpdatedAt, aAsset.UpdatedAtUtc, aErrorMessage)
  else
    Result := True;
end;

class function TMaxGitHubReleaseChecker.TryParseIsoDateTime(const aValue: string; out aDateTime: TDateTime;
  out aErrorMessage: string): Boolean;
begin
  aDateTime := 0;
  aErrorMessage := '';
  Result := True;

  if aValue = '' then
    Exit;

  if not TryISO8601ToDate(aValue, aDateTime, True) then
  begin
    aErrorMessage := 'Invalid ISO8601 timestamp: ' + aValue;
    Result := False;
  end;
end;

class function TMaxGitHubReleaseChecker.TryParseLatestReleaseJson(const aJson: string; out aRelease: TGitHubReleaseInfo;
  out aErrorMessage: string): Boolean;
var
  lAssetIndex: Integer;
  lAssets: TJSONArray;
  lDateText: string;
  lJsonObject: TJSONObject;
  lJsonValue: TJSONValue;
begin
  aRelease := Default(TGitHubReleaseInfo);
  aErrorMessage := '';

  lJsonValue := TJSONObject.ParseJSONValue(aJson);
  if not Assigned(lJsonValue) then
  begin
    aErrorMessage := 'GitHub returned invalid JSON.';
    Exit(False);
  end;

  try
    if not (lJsonValue is TJSONObject) then
    begin
      aErrorMessage := 'GitHub release response is not a JSON object.';
      Exit(False);
    end;

    lJsonObject := TJSONObject(lJsonValue);
    aRelease.TagName := lJsonObject.GetValue<string>('tag_name', '');
    aRelease.Name := lJsonObject.GetValue<string>('name', '');
    aRelease.HtmlUrl := lJsonObject.GetValue<string>('html_url', '');
    aRelease.Draft := lJsonObject.GetValue<Boolean>('draft', False);
    aRelease.Prerelease := lJsonObject.GetValue<Boolean>('prerelease', False);

    if aRelease.TagName = '' then
    begin
      aErrorMessage := 'GitHub release JSON is missing tag_name.';
      Exit(False);
    end;

    lDateText := lJsonObject.GetValue<string>('published_at', '');
    if not TryParseIsoDateTime(lDateText, aRelease.PublishedAtUtc, aErrorMessage) then
      Exit(False);

    lAssets := lJsonObject.Values['assets'] as TJSONArray;
    if Assigned(lAssets) then
    begin
      SetLength(aRelease.Assets, lAssets.Count);
      for lAssetIndex := 0 to lAssets.Count - 1 do
      begin
        if not (lAssets.Items[lAssetIndex] is TJSONObject) then
        begin
          aErrorMessage := Format('GitHub release asset #%d is not a JSON object.', [lAssetIndex]);
          Exit(False);
        end;

        if not TryParseAsset(TJSONObject(lAssets.Items[lAssetIndex]), aRelease.Assets[lAssetIndex], aErrorMessage) then
          Exit(False);
      end;
    end;

    Result := True;
  finally
    lJsonValue.Free;
  end;
end;

class function TMaxGitHubReleaseChecker.TrySplitVersion(const aVersion: string; out aCoreParts: TArray<Int64>;
  out aSuffix: string): Boolean;
var
  lCoreText: string;
  lIndex: Integer;
  lNormalizedVersion: string;
  lPart: string;
  lParts: TArray<string>;
  lSuffixStart: Integer;
  lValue: Int64;
begin
  aCoreParts := nil;
  aSuffix := '';
  lNormalizedVersion := NormalizeVersionTag(aVersion);
  if lNormalizedVersion = '' then
    Exit(False);

  lSuffixStart := 0;
  for lIndex := 1 to Length(lNormalizedVersion) do
    if not CharInSet(lNormalizedVersion[lIndex], ['0'..'9', '.']) then
    begin
      lSuffixStart := lIndex;
      Break;
    end;

  if lSuffixStart > 0 then
  begin
    lCoreText := Copy(lNormalizedVersion, 1, lSuffixStart - 1);
    aSuffix := Copy(lNormalizedVersion, lSuffixStart, MaxInt);
  end else
    lCoreText := lNormalizedVersion;

  while EndsText('.', lCoreText) do
    Delete(lCoreText, Length(lCoreText), 1);

  if lCoreText = '' then
    Exit(False);

  lParts := SplitString(lCoreText, '.');
  SetLength(aCoreParts, Length(lParts));

  for lIndex := 0 to High(lParts) do
  begin
    lPart := Trim(lParts[lIndex]);
    if not TryStrToInt64(lPart, lValue) then
      Exit(False);
    aCoreParts[lIndex] := lValue;
  end;

  Result := True;
end;

function TGitHubReleaseCheckResult.HasRelease: Boolean;
begin
  Result := Status = TGitHubReleaseCheckStatus.gcsSuccess;
end;

function TGitHubReleaseCheckResult.IsUpdateAvailable: Boolean;
begin
  Result := HasRelease and (CurrentVersion <> '') and
    (TMaxGitHubReleaseChecker.CompareVersionTags(LatestRelease.TagName, CurrentVersion) > 0);
end;

function TGitHubReleaseInfo.DisplayName: string;
begin
  if Name <> '' then
    Result := Name
  else
    Result := TagName;
end;

end.
