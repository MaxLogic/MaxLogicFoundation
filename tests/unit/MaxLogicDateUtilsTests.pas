unit MaxLogicDateUtilsTests;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TMaxLogicDateUtilsTests = class
  private
    procedure AssertDateTimeEquals(const aExpected, aActual: TDateTime; const aMessage: string);
  public
    [Test]
    procedure GermanAndIsoWallTimesParseStrictly;
    [Test]
    procedure ExplicitOffsetsPreserveUtcInstantAndBerlinWallTime;
    [Test]
    procedure NaiveBerlinDstGapAndOverlapAreRejected;
    [Test]
    procedure GregorianBoundariesAndMalformedValuesAreHandledStrictly;
    [Test]
    procedure CompatibilityWrapperUsesBerlinWallTime;
    [Test]
    procedure GermanFormattingIsLocaleIndependent;
  end;

implementation

uses
  System.DateUtils, System.SysUtils,
  MaxLogic.DateUtils;

procedure TMaxLogicDateUtilsTests.AssertDateTimeEquals(
  const aExpected, aActual: TDateTime; const aMessage: string);
begin
  Assert.AreEqual(aExpected, aActual, 1 / MSecsPerDay, aMessage);
end;

procedure TMaxLogicDateUtilsTests.GermanAndIsoWallTimesParseStrictly;
var
  lActual: TDateTime;
begin
  Assert.IsTrue(TryParseDateTimeBerlin('22.07.2026', lActual));
  AssertDateTimeEquals(EncodeDate(2026, 7, 22), lActual, 'German date');

  Assert.IsTrue(TryParseDateTimeBerlin('22.07.2026 16:45', lActual));
  AssertDateTimeEquals(EncodeDateTime(2026, 7, 22, 16, 45, 0, 0), lActual, 'German minute');

  Assert.IsTrue(TryParseDateTimeBerlin('22.07.2026 16:45:30.1', lActual));
  AssertDateTimeEquals(EncodeDateTime(2026, 7, 22, 16, 45, 30, 100), lActual, 'German tenth');

  Assert.IsTrue(TryParseDateTimeBerlin('2026-07-22T16:45:30.12', lActual));
  AssertDateTimeEquals(EncodeDateTime(2026, 7, 22, 16, 45, 30, 120), lActual, 'ISO hundredth');

  Assert.IsTrue(TryParseDateTimeBerlin('2026-07-22 16:45:30.123', lActual));
  AssertDateTimeEquals(EncodeDateTime(2026, 7, 22, 16, 45, 30, 123), lActual, 'ISO millisecond');

  Assert.IsTrue(TryParseDateTimeBerlin('2026-07-22T16:45', lActual));
  AssertDateTimeEquals(EncodeDateTime(2026, 7, 22, 16, 45, 0, 0), lActual, 'ISO minute');

  Assert.IsTrue(TryParseDateTimeBerlin('2026-07-22T16:45:30.1', lActual));
  AssertDateTimeEquals(EncodeDateTime(2026, 7, 22, 16, 45, 30, 100), lActual, 'ISO tenth');
end;

procedure TMaxLogicDateUtilsTests.ExplicitOffsetsPreserveUtcInstantAndBerlinWallTime;
var
  lActual: TDateTime;
  lExpectedUtc: TDateTime;
begin
  lExpectedUtc := EncodeDateTime(2026, 7, 22, 14, 45, 30, 123);

  Assert.IsTrue(TryParseDateTimeUtc('2026-07-22T14:45:30.123Z', lActual));
  AssertDateTimeEquals(lExpectedUtc, lActual, 'Zulu instant');

  Assert.IsTrue(TryParseDateTimeUtc('2026-07-22 16:45:30.123+02:00', lActual));
  AssertDateTimeEquals(lExpectedUtc, lActual, 'Positive offset instant');

  Assert.IsTrue(TryParseDateTimeUtc('2026-07-22T12:45:30.123-02:00', lActual));
  AssertDateTimeEquals(lExpectedUtc, lActual, 'Negative offset instant');

  Assert.IsTrue(TryParseDateTimeBerlin('2026-07-22T14:45:30.123Z', lActual));
  AssertDateTimeEquals(EncodeDateTime(2026, 7, 22, 16, 45, 30, 123), lActual, 'Berlin wall time');

  Assert.IsTrue(TryParseDateTimeUtc('2026-10-25T02:30:00+02:00', lActual));
  AssertDateTimeEquals(EncodeDateTime(2026, 10, 25, 0, 30, 0, 0), lActual, 'DST overlap daylight instant');

  Assert.IsTrue(TryParseDateTimeUtc('2026-10-25T02:30:00+01:00', lActual));
  AssertDateTimeEquals(EncodeDateTime(2026, 10, 25, 1, 30, 0, 0), lActual, 'DST overlap standard instant');

  Assert.IsTrue(TryParseDateTimeUtc('2026-07-22T09:15:30-05:30', lActual));
  AssertDateTimeEquals(EncodeDateTime(2026, 7, 22, 14, 45, 30, 0), lActual, 'Half-hour negative offset');

  Assert.IsTrue(TryParseDateTimeUtc('2026-07-22T16:45:30+14:00', lActual));
  AssertDateTimeEquals(EncodeDateTime(2026, 7, 22, 2, 45, 30, 0), lActual, 'Maximum ISO offset');

  Assert.IsTrue(TryParseDateTimeBerlin('2026-03-29T02:30:00+01:00', lActual));
  AssertDateTimeEquals(EncodeDateTime(2026, 3, 29, 3, 30, 0, 0), lActual,
    'An explicit gap offset is an instant, not a naive Berlin wall time');
end;

procedure TMaxLogicDateUtilsTests.NaiveBerlinDstGapAndOverlapAreRejected;
var
  lActual: TDateTime;
begin
  Assert.IsTrue(TryParseDateTimeUtc('2026-03-29T01:59:59', lActual));
  Assert.IsFalse(TryParseDateTimeUtc('2026-03-29T02:00:00', lActual), 'Berlin DST gap start');
  Assert.IsFalse(TryParseDateTimeUtc('2026-03-29T02:30:00', lActual), 'Berlin DST gap');
  Assert.IsFalse(TryParseDateTimeUtc('2026-03-29T02:59:59', lActual), 'Berlin DST gap end');
  Assert.IsTrue(TryParseDateTimeUtc('2026-03-29T03:00:00', lActual));
  Assert.IsTrue(TryParseDateTimeUtc('2026-10-25T01:59:59', lActual));
  Assert.IsFalse(TryParseDateTimeBerlin('2026-03-29 02:30:00', lActual), 'Berlin DST gap wall time');
  Assert.IsFalse(TryParseDateTimeUtc('2026-10-25T02:30:00', lActual), 'Berlin DST overlap');
  Assert.IsFalse(TryParseDateTimeUtc('2026-10-25T02:00:00', lActual), 'Berlin DST overlap start');
  Assert.IsFalse(TryParseDateTimeUtc('2026-10-25T02:59:59', lActual), 'Berlin DST overlap end');
  Assert.IsFalse(TryParseDateTimeBerlin('25.10.2026 02:30:00', lActual), 'Berlin DST overlap wall time');
  Assert.IsTrue(TryParseDateTimeUtc('2026-10-25T03:00:00', lActual));
end;

procedure TMaxLogicDateUtilsTests.GregorianBoundariesAndMalformedValuesAreHandledStrictly;
var
  lActual: TDateTime;
  lValue: string;
  lValues: TArray<string>;
begin
  Assert.IsTrue(TryParseDateTimeBerlin('0001-01-01', lActual), 'Year 0001');
  AssertDateTimeEquals(EncodeDate(1, 1, 1), lActual, 'First Gregorian year');
  Assert.IsTrue(TryParseDateTimeBerlin('9999-12-31', lActual), 'Year 9999');
  AssertDateTimeEquals(EncodeDate(9999, 12, 31), lActual, 'Last four-digit year');
  Assert.IsFalse(TryParseDateTimeUtc('0001-01-01T00:00:00+14:00', lActual),
    'Explicit offset must not underflow the four-digit UTC range');
  Assert.IsFalse(TryParseDateTimeUtc('9999-12-31T23:59:59-14:00', lActual),
    'Explicit offset must not overflow the four-digit UTC range');
  Assert.IsFalse(TryParseDateTimeUtc('0001-01-01T00:00:00', lActual),
    'Berlin-local time must not underflow the four-digit UTC range');
  Assert.IsFalse(TryParseDateTimeBerlin('9999-12-31T23:30:00Z', lActual),
    'UTC time must not overflow the four-digit Berlin wall-time range');

  lValues := [
    '', '0000-01-01', '10000-01-01', '2026-00-22', '2026-13-22',
    '2026-04-31', '2026-07-22T24:00', '2026-07-22T16:60',
    '2026-07-22T16:45:60', '2026-07-22T16:45:30.1234',
    '2026-07-22T16:45.123',
    '2026-07-22Z', '2026-07-22T16:45+0200', '2026-07-22T16:45+14:01',
    '2026-07-22T16:45+19:00', '2026-07-22t16:45', '2026-07-22T16:45z',
    '22.07.2026T16:45', '2026-07-22T16:45 trailing'
  ];
  for lValue in lValues do
    Assert.IsFalse(TryParseDateTimeBerlin(lValue, lActual), 'Must reject: ' + lValue);
end;

procedure TMaxLogicDateUtilsTests.CompatibilityWrapperUsesBerlinWallTime;
begin
  AssertDateTimeEquals(
    EncodeDateTime(2026, 7, 22, 16, 45, 30, 0),
    ParseDateTime('2026-07-22T14:45:30Z'),
    'ParseDateTime must retain its wall-time contract');
  Assert.AreEqual(TDateTime(0), ParseDateTime('not-a-date'));
end;

procedure TMaxLogicDateUtilsTests.GermanFormattingIsLocaleIndependent;
begin
  Assert.AreEqual(
    '22.07.2026 16:45:30',
    FormatGermanDateTime(EncodeDateTime(2026, 7, 22, 16, 45, 30, 987)));
  Assert.AreEqual(
    '22.07.2026 16:45:30',
    FormatGermanDateTimeUtc(EncodeDateTime(2026, 7, 22, 14, 45, 30, 987)),
    'Summer UTC must be formatted as Europe/Berlin daylight time.');
  Assert.AreEqual(
    '22.01.2026 15:45:30',
    FormatGermanDateTimeUtc(EncodeDateTime(2026, 1, 22, 14, 45, 30, 987)),
    'Winter UTC must be formatted as Europe/Berlin standard time.');
end;

initialization
  TDUnitX.RegisterTestFixture(TMaxLogicDateUtilsTests);

end.
