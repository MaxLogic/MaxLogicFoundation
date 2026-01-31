unit MaxLogic.AsyncLoop.Tests;

{$I fpc_delphimode.inc}

interface

uses
  System.SysUtils,
  System.SyncObjs,
  System.Classes,
  DUnitX.TestFramework,
  maxAsync;

type
  [TestFixture]
  TAsyncLoopTests = class
  public

    [Test] procedure RunSignalsOnDoneWithoutDeadlock;
    [Test] procedure RunCancelsEarly;
    [Test] procedure RunMultiThreadCompletes;
    [Test] procedure RunStressCompletes;
  end;

implementation

[Test]
procedure TAsyncLoopTests.RunSignalsOnDoneWithoutDeadlock;
const
  cTimeoutMs = 2000;
var
  lDone: TEvent;
  lIterations: Integer;
begin
  lDone := TEvent.Create(nil, True, False, '');
  try
    lIterations := 0;
    TAsyncLoop.Run(0, 0,
      procedure(CurIndex: integer; var aCancel: boolean)
      begin
        TInterlocked.Increment(lIterations);
        aCancel := False;
      end,
      procedure
      begin
        lDone.SetEvent;
      end,
      1);

    Assert.AreEqual<TWaitResult>(wrSignaled, lDone.WaitFor(cTimeoutMs),
      'OnDone should be signaled quickly');
    Assert.AreEqual<Integer>(1, lIterations, 'Loop body should execute once');
  finally
    lDone.Free;
  end;
end;

[Test]
procedure TAsyncLoopTests.RunCancelsEarly;
const
  cTimeoutMs = 2000;
var
  lDone: TEvent;
  lIterations: Integer;
  lMax: Integer;
begin
  lDone := TEvent.Create(nil, True, False, '');
  try
    lIterations := 0;
    lMax := 5000;
    TAsyncLoop.Run(0, lMax,
      procedure(CurIndex: integer; var aCancel: boolean)
      begin
        TInterlocked.Increment(lIterations);
        if CurIndex = 0 then
          aCancel := True
        else
          aCancel := False;
      end,
      procedure
      begin
        lDone.SetEvent;
      end,
      2);

    Assert.AreEqual<TWaitResult>(wrSignaled, lDone.WaitFor(cTimeoutMs),
      'OnDone should be signaled quickly after cancel');
    Assert.IsTrue(lIterations < lMax, 'Cancel should stop iterations early');
  finally
    lDone.Free;
  end;
end;

[Test]
procedure TAsyncLoopTests.RunMultiThreadCompletes;
const
  cTimeoutMs = 2000;
var
  lDone: TEvent;
  lIterations: Integer;
  lMax: Integer;
begin
  lDone := TEvent.Create(nil, True, False, '');
  try
    lIterations := 0;
    lMax := 2000;
    TAsyncLoop.Run(0, lMax,
      procedure(CurIndex: integer; var aCancel: boolean)
      begin
        TInterlocked.Increment(lIterations);
        aCancel := False;
      end,
      procedure
      begin
        lDone.SetEvent;
      end,
      4);

    Assert.AreEqual<TWaitResult>(wrSignaled, lDone.WaitFor(cTimeoutMs),
      'OnDone should be signaled for multi-threaded run');
    Assert.AreEqual<Integer>(lMax + 1, lIterations, 'All iterations should run');
  finally
    lDone.Free;
  end;
end;

[Test]
procedure TAsyncLoopTests.RunStressCompletes;
const
  cTimeoutMs = 8000;
var
  lDone: TEvent;
  lIterations: Integer;
  lMax: Integer;
begin
  lDone := TEvent.Create(nil, True, False, '');
  try
    lIterations := 0;
    lMax := 20000;
    TAsyncLoop.Run(0, lMax,
      procedure(CurIndex: integer; var aCancel: boolean)
      begin
        TInterlocked.Increment(lIterations);
        aCancel := False;
      end,
      procedure
      begin
        lDone.SetEvent;
      end,
      8);

    Assert.AreEqual<TWaitResult>(wrSignaled, lDone.WaitFor(cTimeoutMs),
      'OnDone should be signaled for stress run');
    Assert.AreEqual<Integer>(lMax + 1, lIterations, 'All iterations should run');
  finally
    lDone.Free;
  end;
end;

end.
