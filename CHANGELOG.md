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
