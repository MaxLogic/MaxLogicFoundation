unit MaxLogic.CmdLineParams.Tests;

interface

uses
  System.SysUtils,
  DUnitX.TestFramework,
  maxLogic.CmdLineParams;

type
  [TestFixture]
  TCmdLineParamsTests = class
  private
    procedure AssertValue(const aCommandLine, aSwitch, aExpected: string);
  public
    [Test] procedure SetParam_PreservesDriveQualifiedValue;
    [Test] procedure AttachedValues_PreserveEmbeddedDelimiters;
    [Test] procedure SeparateEmptyDuplicateAndNegativeValues;
    [Test] procedure PrefixAndCaseBehavior;
    [Test] procedure ExplicitSwitchPrefixes_AreApplied;
  end;

implementation

procedure TCmdLineParamsTests.AssertValue(const aCommandLine, aSwitch, aExpected: string);
var
  lParams: iCmdLineparams;
  lValue: string;
begin
  lParams := TCmdLineParams.Create;
  lParams.BuildFromString(aCommandLine);
  Assert.IsTrue(lParams.Find(aSwitch, lValue), aCommandLine);
  Assert.AreEqual<string>(aExpected, lValue, aCommandLine);
end;

procedure TCmdLineParamsTests.SetParam_PreservesDriveQualifiedValue;
var
  lParams: iCmdLineparams;
  lValue: string;
begin
  lParams := TCmdLineParams.Create;
  lParams.SetParam('--out', 'D:\tmp\backup');
  Assert.IsTrue(lParams.Find('out', lValue));
  Assert.AreEqual<string>('D:\tmp\backup', lValue);
end;

procedure TCmdLineParamsTests.AttachedValues_PreserveEmbeddedDelimiters;
begin
  AssertValue('--out=D:\tmp\a:b=c', 'out', 'D:\tmp\a:b=c');
  AssertValue('--out:D:\tmp\a=b', 'out', 'D:\tmp\a=b');
  AssertValue('--url:https://host/path?a=b', 'url', 'https://host/path?a=b');
  AssertValue('--name=foo:bar=baz', 'name', 'foo:bar=baz');
end;

procedure TCmdLineParamsTests.SeparateEmptyDuplicateAndNegativeValues;
begin
  AssertValue('--out "D:\tmp\a=b"', 'out', 'D:\tmp\a=b');
  AssertValue('--out=', 'out', '');
  AssertValue('--out=first --out=second', 'out', 'first');
  AssertValue('--count -5', 'count', '-5');
end;

procedure TCmdLineParamsTests.PrefixAndCaseBehavior;
var
  lParams: iCmdLineparams;
  lValue: string;
begin
  lParams := TCmdLineParams.Create;
  lParams.BuildFromString('-OuT:value /other=second');
  Assert.IsTrue(lParams.Find('out', lValue));
  Assert.AreEqual<string>('value', lValue);
  Assert.IsFalse(lParams.Find('out', False));
  Assert.IsTrue(lParams.Find('OuT', False));
  {$IFDEF MSWINDOWS}
  Assert.IsTrue(lParams.Find('other', lValue));
  Assert.AreEqual<string>('second', lValue);
  {$ELSE}
  Assert.IsFalse(lParams.Find('other', lValue));
  {$ENDIF}
end;

procedure TCmdLineParamsTests.ExplicitSwitchPrefixes_AreApplied;
var
  lParams: iCmdLineparams;
  lValue: string;
begin
  lParams := TCmdLineParams.Create(nil, [spDash]);
  lParams.BuildFromString('-out=ok --ignored=double /ignored=slash');
  Assert.IsTrue(lParams.Find('out', lValue));
  Assert.IsTrue(lParams.SwitchPrefixes = [spDash]);
  Assert.AreEqual<string>('ok', lValue);
  Assert.IsFalse(lParams.Find('ignored'));
end;

end.
