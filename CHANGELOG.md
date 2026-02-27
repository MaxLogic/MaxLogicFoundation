# Changelog

All notable changes to MaxLogicFoundation are documented here.

## [Unreleased]

### Added

- `MaxLogic.Cache`: multi-namespace, single-flight cache with TTL/idle expiration, tag invalidation, lazy dependency validation, sweeper eviction, metrics, and fail-fast shutdown.
- `readme.cache.md`: usage and behavior notes for `MaxLogic.Cache`.

### Fixed

- Cache invalidation and eviction now mark entries obsolete before bucket removal to avoid tag pollution under concurrent loads.
- `TMaxCache.ScopedTag` now rejects tags containing `|` unless they are `global:`.
- Load-time histogram bounds are unified between recording and P95 estimation.
- Tag registration and unregistration now run under the entry monitor to avoid ghost tags during concurrent invalidation.
- Waiting timeouts now mark entries failed and set a retry window to prevent stuck `Loading` entries.
- Win32 builds use atomic reads/writes for entry Int64 timestamps and size estimates to avoid torn reads.
- `maxAsync.TAsyncTimer` is now deprecated and internally delegates to `MaxLogic.PortableTimer.TPortableTimer`; new timer usage should use `MaxLogic.PortableTimer` directly.
- `maxAsync.TAsyncLoop` and `TAsyncCollectionProcessor<T>` now guarantee completion signaling even when worker callbacks raise exceptions, preventing `WaitFor` stalls.
- `maxAsync.TAsyncCollectionProcessor<T>` now keeps ready-state transitions consistent when `OnFinished` enqueues new work, so `WaitFor` no longer reports completion prematurely.
- `maxAsync.TSafeValue<T>` now uses thread-safe first-touch initialization under concurrency.
- `maxAsync.TLockFreeLock` now yields under contention (periodic `Sleep(0)`) instead of pure tight busy-spinning.
- `maxAsync.TAsyncCollectionProcessor<T>.Add(...)` now fails fast when `Proc` is not assigned to prevent silent item loss.
- `maxAsync.TAsyncCollectionProcessor<T>.WaitFor` is now safe when called from `OnFinished` on the same processor (no self-deadlock).
- `maxAsync.TAsyncLoop.Cancel` is now idempotent and overflow-safe for large ranges.
- `maxAsync.TWaiter.WaitFor(iAsync[])` now validates internal async compatibility explicitly and raises a clear error instead of relying on hard casts.
- `maxAsync.TAtomic` alignment guard is now an explicit compile-time error when the required A8/A+ alignment mode is not active.
- `maxAsync.TAsyncCollectionProcessor<T>.Add(...)` now starts workers inside the same critical-section pass used for enqueue, reducing lock churn in hot paths.
- `maxAsync.TAsyncCollectionProcessor<T>.AsyncProcessItem` now invokes a captured procedure reference per dequeued item, avoiding repeated branch checks in the worker loop.
- `maxAsync.TAsyncCollectionProcessor<T>` now dequeues work in small batches and avoids redundant ready-event resets, improving processing throughput under high enqueue rates.
