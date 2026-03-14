unit MaxLogic.GitHubReleaseChecker.Tests;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TMaxLogicGitHubReleaseCheckerTests = class
  public
    [Test] procedure LatestReleaseUrl_UsesOwnerAndRepository;
    [Test] procedure TryParseLatestReleaseJson_ExtractsReleaseFields;
    [Test] procedure TryParseLatestReleaseJson_InvalidJson_ReturnsFalse;
    [Test] procedure CompareVersionTags_HandlesLeadingVAndNumericSegments;
    [Test] procedure CompareVersionTags_ReleaseIsNewerThanPrerelease;
  end;

implementation

uses
  System.DateUtils,
  System.SysUtils,

  MaxLogic.GitHubReleaseChecker;

procedure TMaxLogicGitHubReleaseCheckerTests.CompareVersionTags_HandlesLeadingVAndNumericSegments;
begin
  Assert.IsTrue(TMaxGitHubReleaseChecker.CompareVersionTags('v1.2.10', '1.2.9') > 0);
  Assert.AreEqual(0, TMaxGitHubReleaseChecker.CompareVersionTags('v1.2.0', '1.2'));
  Assert.IsTrue(TMaxGitHubReleaseChecker.CompareVersionTags('1.2.0', '1.10.0') < 0);
end;

procedure TMaxLogicGitHubReleaseCheckerTests.CompareVersionTags_ReleaseIsNewerThanPrerelease;
begin
  Assert.IsTrue(TMaxGitHubReleaseChecker.CompareVersionTags('1.2.0', '1.2.0-beta.1') > 0);
  Assert.IsTrue(TMaxGitHubReleaseChecker.CompareVersionTags('1.2.0-rc.1', '1.2.0-beta.1') > 0);
end;

procedure TMaxLogicGitHubReleaseCheckerTests.LatestReleaseUrl_UsesOwnerAndRepository;
var
  lChecker: TMaxGitHubReleaseChecker;
begin
  lChecker := TMaxGitHubReleaseChecker.Create('MaxLogic', 'AgentSkillSearch');
  try
    Assert.AreEqual(
      'https://api.github.com/repos/MaxLogic/AgentSkillSearch/releases/latest',
      lChecker.LatestReleaseUrl);
  finally
    lChecker.Free;
  end;
end;

procedure TMaxLogicGitHubReleaseCheckerTests.TryParseLatestReleaseJson_ExtractsReleaseFields;
const
  cResponse =
    '{' +
    '"tag_name":"v1.4.0",' +
    '"name":"Spring Cleanup",' +
    '"html_url":"https://github.com/MaxLogic/AgentSkillSearch/releases/tag/v1.4.0",' +
    '"published_at":"2026-03-13T10:11:12Z",' +
    '"draft":false,' +
    '"prerelease":false,' +
    '"assets":[' +
      '{' +
      '"name":"AgentSkillSearch.zip",' +
      '"browser_download_url":"https://github.com/MaxLogic/AgentSkillSearch/releases/download/v1.4.0/AgentSkillSearch.zip",' +
      '"content_type":"application/zip",' +
      '"size":123456,' +
      '"updated_at":"2026-03-13T10:15:00Z"' +
      '}' +
    ']' +
    '}';
var
  lRelease: TGitHubReleaseInfo;
  lErrorMessage: string;
  lYear, lMonth, lDay: Word;
  lHour, lMinute, lSecond, lMilliSecond: Word;
begin
  Assert.IsTrue(TMaxGitHubReleaseChecker.TryParseLatestReleaseJson(cResponse, lRelease, lErrorMessage), lErrorMessage);
  Assert.AreEqual('', lErrorMessage);
  Assert.AreEqual('v1.4.0', lRelease.TagName);
  Assert.AreEqual('Spring Cleanup', lRelease.Name);
  Assert.AreEqual(
    'https://github.com/MaxLogic/AgentSkillSearch/releases/tag/v1.4.0',
    lRelease.HtmlUrl);
  Assert.IsFalse(lRelease.Draft);
  Assert.IsFalse(lRelease.Prerelease);
  Assert.AreEqual(1, Length(lRelease.Assets));
  Assert.AreEqual('AgentSkillSearch.zip', lRelease.Assets[0].Name);
  Assert.AreEqual('application/zip', lRelease.Assets[0].ContentType);
  Assert.AreEqual(Int64(123456), lRelease.Assets[0].Size);

  DecodeDateTime(lRelease.PublishedAtUtc, lYear, lMonth, lDay, lHour, lMinute, lSecond, lMilliSecond);
  Assert.AreEqual(2026, Integer(lYear));
  Assert.AreEqual(3, Integer(lMonth));
  Assert.AreEqual(13, Integer(lDay));
  Assert.AreEqual(10, Integer(lHour));
  Assert.AreEqual(11, Integer(lMinute));
  Assert.AreEqual(12, Integer(lSecond));
end;

procedure TMaxLogicGitHubReleaseCheckerTests.TryParseLatestReleaseJson_InvalidJson_ReturnsFalse;
var
  lRelease: TGitHubReleaseInfo;
  lErrorMessage: string;
begin
  Assert.IsFalse(TMaxGitHubReleaseChecker.TryParseLatestReleaseJson('{"tag_name":', lRelease, lErrorMessage));
  Assert.IsTrue(lErrorMessage <> '');
end;

initialization
  TDUnitX.RegisterTestFixture(TMaxLogicGitHubReleaseCheckerTests);

end.
