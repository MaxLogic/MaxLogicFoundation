# maxAsync Architecture and Performance

## Scope

`maxAsync.pas` provides:

- `SimpleAsyncCall(...)` / `iAsync`
- `TAsyncLoop`
- `TAsyncCollectionProcessor<T>`
- `TAsyncTimer` compatibility wrapper

`TAsyncTimer` is legacy compatibility only. For new timer work, use `MaxLogic.PortableTimer.TPortableTimer`.

## Current Architecture

### `SimpleAsyncCall`

- Reuses long-lived worker threads (`TThreadData`) instead of one thread per call.
- Uses global waiting-pool reuse and a main-thread local cache fast path for lower create/destroy churn.
- Keeps `WakeUp(...)` path for reuse-heavy scenarios (`iAsync.WakeUp`).

### `TAsyncLoop`

- Executes through RTL `TParallel.For`.
- Caches custom `TThreadPool` instances per requested thread count, avoiding repeated pool setup overhead.

### `TAsyncCollectionProcessor<T>`

- Uses persistent workers started once and reused.
- Supports queue modes:
  - `acpqmLegacyLockedQueue`
  - `acpqmLockedRingQueue`
  - `acpqmLockFreeMpmcRingQueue`
- Uses atomic pending/completion signaling semantics that keep `WaitFor` and `OnFinished` behavior stable.
- For `acpqmLegacyLockedQueue`, effective worker count is capped at `2` to reduce lock contention on the central queue critical section.

## Benchmark Method

Command:

```bash
cd tests
./build-delphi.sh ../benchmarks/MaxLogic.Async.Benchmark.dproj
../benchmarks/Win32/MaxLogic.Async.Benchmark.exe --warmup=1 --repeats=5
```

- Baseline reference: commit `19b8a04` (before latest architecture/perf optimizations)
- Current reference: three fresh benchmark invocations on `2026-03-03`, each median-of-5; values below use median across those three invocations.

## Performance Gains (Baseline -> Current)

| Metric | Baseline ops/s | Current ops/s | Delta |
|---|---:|---:|---:|
| `simple_async_call_ops_per_sec` | 59,476 | 57,464 | -3.4% |
| `async_collection_processor_ops_per_sec` | 280,554 | 633,672 | +125.9% |
| `async_collection_processor_batched_ops_per_sec` | 1,942,493 | 2,783,197 | +43.3% |
| `async_collection_processor_multi_producer_ops_per_sec` | 468,392 | 1,526,465 | +225.9% |

## RTL (`TTask`) Comparison (Current)

From the same three fresh runs (`2026-03-03`):

- `TAsyncCollectionProcessor` vs `TTask + TThreadedQueue`: `633,672` vs `209,318` ops/s (`~3.03x` faster)
- `TAsyncLoop.RunAndWait` vs `TParallel.For`: `57,571,629` vs `58,255,821` ops/s (`~98.8%` of RTL)
- `SimpleAsyncCall` vs `TTask.Run` (sequential wait): `57,464` vs `147,408` ops/s (`~39.0%` of RTL)
- `SimpleAsyncCall.WakeUpReuse` vs `TTask.Run`: `72,114` vs `147,408` ops/s (`~48.9%` of RTL)

## Practical Interpretation

- `TAsyncCollectionProcessor<T>` is now materially faster than the `TTask + TThreadedQueue` baseline for this workload.
- `TAsyncLoop` is near RTL `TParallel.For` throughput.
- `SimpleAsyncCall` still trades raw one-shot throughput for richer reuse-oriented semantics; it performs best when reused through `WakeUp(...)`.
