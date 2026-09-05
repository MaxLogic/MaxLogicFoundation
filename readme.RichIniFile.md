# TRichIniFile – Rich INI reader/writer

`TRichIniFile` is a Delphi 12+ drop-in friendly INI engine that keeps the source file intact while exposing a familiar `TIniFile`/`TMemIniFile` surface. It now descends from `System.IniFiles.TCustomIniFile`, so APIs that expect a `TCustomIniFile` instance can use it directly. It is designed for tooling and configuration scenarios that need comment preservation, duplicate keys, controlled encodings, and atomic saves without sacrificing performance.

## Highlights
- Preserves the original text layout: comments, blank lines, duplicate sections/keys, and key ordering are kept byte-for-byte unless you mutate them.
- Comment ownership modes (`coAttachToNext`, `coAttachToPrev`, `coNone`) let you control where runs of comments travel during edits.
- Duplicate-aware API: append keys, enumerate all duplicate values, ask for the last occurrence index, and consolidate sections when you need a single view.
- Encoding and newline control: auto-detect BOM/encoding on load, force UTF-8/ANSI/custom encodings on save, and override newline style per spec.
- `TCustomIniFile` compatibility: `UpdateFile`, `ReadBool`, `WriteBool`, and the standard INI read/write surface work through the RichIni engine.
- Atomic `SaveToFile`: writes to a sibling temp file and renames into place, mirroring the detected BOM and newline semantics unless overridden.
- Compatibility surface mirrors `TMemIniFile` (`ReadString`, `WriteString`, `ReadSection*`, `EraseSection`, etc.), so existing consumers can migrate with minimal changes.

## Construction & Options

```pascal
var
  Options: TRichIniOptions;
  Ini: TRichIniFile;
begin
  Options := DefaultRichIniOptions;
  Options.CaseSensitivity := csCaseInsensitive;
  Ini := TRichIniFile.Create('settings.ini', Options);
  try
    // work with Ini
  finally
    Ini.Free;
  end;
end;
```

Key option knobs (see `MaxLogic.RichIniFile.pas` for full detail):

| Option | Purpose |
| --- | --- |
| `LoadEncoding` / `SaveEncoding` | Choose between auto-detect, UTF-8, ANSI, or a caller-supplied `TEncoding`. |
| `BomPolicy` | Reuse the source BOM, force one, or suppress it entirely. |
| `NewlineMode` | Preserve the input newline convention, use the platform default, or force CRLF/LF. |
| `CaseSensitivity` | Toggle between case-sensitive (`Ordinal`) and case-insensitive (`OrdinalIgnoreCase`) lookups. |
| `AcceptMissingBracket` | Decide how to treat malformed section headers (`[Section`). |
| `CommentPrefixes` | Customize which prefixes (`;`, `#`, `//` by default) are treated as full-line comments. |
| `CommentOwnership` | Route comment runs to their next/previous owner or keep them orphaned. |
| `KeyValueDelimiter` | Accept alternate delimiters such as `:` or space. |
| `BooleanTrueValues` | Case-insensitive tokens treated as `True` when reading booleans (`1`, `y`, `yes`, `on`, `enabled`, `true` by default). |
| `BooleanTrueValue` / `BooleanFalseValue` | Tokens written by `WriteBool` (`1` / `0` by default). |

Use `CreateFromStrings` when you already have the text in memory and want to avoid filesystem IO:

```pascal
Ini := TRichIniFile.CreateFromStrings(SourceLines, Options);
```

Use `LoadFromText` when we already decoded the file content ourselves and still want `TRichIniFile` to preserve the source newline/BOM/encoding semantics for later `SaveToFile` calls:

```pascal
Ini.LoadFromText(SourceText, TEncoding.UTF8, True);
```

Use `TRichIniFile` anywhere a `TCustomIniFile` is expected:

```pascal
var
  IniBase: TCustomIniFile;
begin
  IniBase := TRichIniFile.Create('settings.ini', Options);
  try
    IniBase.WriteBool('Main', 'Enabled', True); // writes "1" by default
    IniBase.UpdateFile;
  finally
    IniBase.Free;
  end;
end;
```

## Reading & writing

```pascal
Value := Ini.ReadString('Database', 'User', 'fallback');
Ini.WriteString('Database', 'Password', 'secret');
Ini.SaveToFile; // optional: path argument overrides the constructor target
```

Compatibility methods collapse duplicates (last occurrence wins), mirroring RTL semantics. Rich extensions expose the duplicate stream:

- `AppendKey` → appends a new key occurrence and returns its index.
- `ReadAllKeyValues` → fetches every value for a duplicate key.
- `LastKeyIndex`, `KeyCount` → track how many duplicates exist and where the last one lives.
- `ReadComment` / `WriteComment` / `PurgeComments` → inspect or mutate comment blocks.
- `ConsolidateSection` / `ConsolidateAll` → squashes duplicate keys while keeping the newest values.
- `ReadMultilineString` / `WriteMultilineString` → encode/decode multi-line payloads using the spec’s helper format.

Global sections are supported (`aSection = ''`) and never emit headers on save. Section/Key order is preserved by keeping an ordered list alongside lookup dictionaries, giving `O(1)` lookups and predictable save ordering.

## Persistence semantics

- Loading detects BOM and newline style once, normalizes section/key tokens through `TFastCaseAwareComparer.Ordinal` /
  `TFastCaseAwareComparer.OrdinalIgnoreCase`, and tracks dirty state.
- Writes mutate the in-memory AST and mark affected lines “dirty”; untouched lines are emitted verbatim.
- `SaveToFile` resolves the outgoing encoding/newline according to the option set, writes to a temporary file in the destination folder, then replaces the target atomically.

## Limitations

- Inline (same-line) comments remain unsupported; the parser keeps them as part of the value.
- INI values are single-line by nature; for multi-line text we use the built-in helpers `WriteMultilineString` / `ReadMultilineString`.
  These methods store line breaks inside the value using a simple escape format: newlines become `\n` and literal backslashes become `\\`.
- Options are set at construction time in the current public API; create a fresh instance if we need different comparers or comment behavior.

## Testing & benchmarks

- Unit tests: `tests/unit/MaxLogic.RichIniFiles.Tests.pas` (registered in `tests/MaxLogic.Tests.dpr`). Run them via `./tests/build-tests.sh` followed by `./tests/MaxLogic.Tests.exe`.
- Benchmarks: `benchmarks/MaxLogic.RichIniBenchmark.dpr` compares `TRichIniFile`, `TMemIniFile`, and `TIniFile` across load/read/write/save workloads and multiple INI sizes (Tiny/Small/Medium/Large). Build with `./tests/build-delphi.sh benchmarks/MaxLogic.RichIniBenchmark.dproj` and run `benchmarks/Win32/MaxLogic.RichIniBenchmark.exe` (CLI: `--iterations=N`, `--warmup=N`).

## Performance notes (what the benchmark shows)

Observations from our benchmark runs (15 iterations, warmup=3) with INIs in the ~2 KB → ~160 KB range:

- **Load:** `TRichIniFile` is typically ~1.5×–2× slower than `TMemIniFile` because we build a richer model (preserve ordering, comments, duplicates).
- **Read:** `TRichIniFile` is typically ~2×–4× faster than `TMemIniFile` for repeated `ReadString` calls (dictionary lookup dominates).
- **Write (in-memory):** `WriteString` is effectively O(1) and very cheap; absolute timings are tiny and can be noisy.
- **Save:** for small INIs, total save time is often dominated by our atomic-save path (temp file + replace + flush), so it looks almost “flat” across Tiny/Small/Medium sizes and has higher variance. As the file grows, the gap vs `TMemIniFile.UpdateFile` narrows.

Rule of thumb: if our workload is “load once, read many keys”, `TRichIniFile` typically breaks even after only a handful of reads. If our workload is “write + save in a tight loop”, `TMemIniFile` will usually stay faster.

## When to choose TRichIniFile

Pick `TRichIniFile` when you need any of:
- Preservation of layout and comments for round-tripping user-edited INI files.
- Duplicate section/key awareness beyond “last wins”.
- Strong control over encoding/newline policy during save.
- Comment ownership that survives edits without manual bookkeeping.
- Atomic saves to avoid partial writes or file corruption on failure.

Stick to `TIniFile` or `TMemIniFile` when we only need quick key/value access and do not care about formatting or duplicates (or when we need to save extremely frequently and want the lowest possible save latency).

## Native UTF8 version

`MaxLogic.RichIniFile.Utf8` provides `TRichIniFileUtf8` and `TRichIniOptionsUtf8`.
The original `MaxLogic.RichIniFile.TRichIniFile` keeps its UnicodeString API and
`TCustomIniFile` inheritance. Both compile the document algorithms from
`MaxLogic.RichIniFile.Shared.inc`; native UTF8 primitives live in
`MaxLogic.RichIniFile.Utf8.Helpers.inc`. No project define is required, and both
units can be linked into the same application.

```delphi
uses
  AutoFree, MaxLogic.RichIniFile.Utf8;

function ReadUtf8Setting(const aText, aSection, aKey: UTF8String): UTF8String;
var
  g: TGarbos;
  lIni: TRichIniFileUtf8;
  lOptions: TRichIniOptionsUtf8;
begin
  lOptions := MaxLogic.RichIniFile.Utf8.DefaultRichIniOptions;
  GC(lIni, TRichIniFileUtf8.Create('', lOptions), g);
  lIni.LoadFromText(aText);
  Result := lIni.ReadString(aSection, aKey, '');
end;
```

For repeated access, keep the instance and load once. Document text, section
names, keys, values, comments and dictionary keys stay UTF8String. Ordinary
warmed string reads and the tested integer/boolean reads allocate no heap blocks.
Writes allocate their new text but do not convert document strings to UTF16.

- `ReadSection`, `ReadSections` and `ReadSectionValues` fill a caller-owned
  `TList<UTF8String>` (also named `TRichIniStrings` in the UTF8 unit).
  `ReadAllKeyValues` returns `TArray<UTF8String>` through its existing out parameter.
- Filenames stay UnicodeString to match Delphi and Windows filesystem APIs.
- `KeyValueDelimiter` stays `Char`; non-ASCII BMP delimiters are encoded once
  and matched as complete UTF8 sequences.
- Case-sensitive lookup is ordinal byte comparison. Case-insensitive lookup
  and boolean tokens use our native `TFastCaseAwareComparer` UTF8 variants:
  simple BMP uppercase mapping, no locale collation or Unicode normalization,
  with supplementary code points kept case-sensitive.
- Native UTF8 load/save preserves malformed bytes as opaque data. The native
  `eoAutoDetect` mode recognizes BOMs and defaults to UTF8 without a BOM.
  The existing Unicode version retains its system-encoding fallback.
- ANSI, UTF16 and explicitly custom encodings convert at the file boundary.
  A supplied custom decoder is honored, including its decoding errors.
  Custom encoding objects remain caller-owned and must outlive the INI object.
- The native class is independent of `TCustomIniFile`. The methods explicitly
  declared by RichIni are available; inherited RTL conveniences such as
  `ReadDate`, `ReadFloat` and `ReadBinaryStream` are not part of its API.
- When importing both units, use the suffixed native class/options names and
  qualify `DefaultRichIniOptions` with the intended unit. The option records
  belong to their respective string versions.

### UTF8 performance proof

Measured on 2026-09-05 with Delphi 12 Release, outside the debugger, on this
Windows development machine. The retained benchmark exercises 100 and 10,000
keys, ASCII and non-ASCII text, both case modes, and short/256-byte-padded values.
Each comparison uses two warmups and 15 measured samples, alternating execution
order. Construction, input generation, document resets, logging and correctness
checks are outside timing. The Unicode baseline includes conversions from and
back to actual UTF8 caller variables. Writes update existing values on every pass.
File load/save includes filesystem work and atomic replacement.

Representative Win64 result: 10,000 keys, non-ASCII text, case-insensitive lookup.
All times below are milliseconds; p95 uses nearest rank (the maximum of 15 samples).

| Operation | Unicode median | UTF8 median | Unicode p95 | UTF8 p95 |
| --- | ---: | ---: | ---: | ---: |
| 50,000 reads | 86.624 | 37.776 | 109.225 | 43.941 |
| 20,000 writes | 44.854 | 20.485 | 49.553 | 25.173 |
| Load text | 24.589 | 17.769 | 28.234 | 20.391 |
| Load file | 25.576 | 20.322 | 31.633 | 25.479 |
| Atomic save | 7.793 | 4.144 | 9.966 | 25.290 |

Across the eight scenarios, native reads were 1.70-4.34 times as fast on Win32
and 1.71-4.76 times as fast on Win64. Native writes were 1.51-2.66 and 1.65-2.74
times as fast respectively. Small file operations remain dominated by filesystem
costs; some were slower, and the representative atomic-save p95 above regressed.
These measurements establish the benefit for repeated access, not a universal
speed guarantee for every INI workload.

Reproduce the benchmark from the repository root (replace Win64 with Win32 for
that platform):

```powershell
& $env:DAK_EXE build --project benchmarks\MaxLogic.RichIniUtf8.Benchmark.dproj --delphi 23.0 --platform Win64 --config Release --show-warnings --ai
& benchmarks\Win64\Release\MaxLogic.RichIniUtf8.Benchmark.exe
```

The benchmark emits CSV with min/median/p95/max and returns a nonzero exit code
if a checked read, write, load or saved-byte result is incorrect. It runs each
platform separately and uses unique temporary input/output files.

Tests cover both implementations, duplicate/order/comment behavior, native
case handling, scalar parsing, multiline escapes, raw bytes, custom decoders,
non-ASCII delimiters, missing/error paths and 48 encoding/BOM/newline combinations.
The UTF8 read regression originally measured 300 allocations for 100 reads
through the Unicode boundary; the native path measures zero. Both fixtures
also passed in an isolated Win64 Debug runner with range/overflow checking.

From `tests`, run the focused Win32 suites after a Release build:

```powershell
.\build-delphi.bat MaxLogic.Tests.dproj -config Release -no-brand -show-warnings-on-success
$env:MAXLOGIC_MADEXCEPT_AI = '1'
.\MaxLogic.Tests.exe --run:MaxLogic.RichIniFiles.Tests.TRichIniFilesTests --exitbehavior:Continue --consolemode:Quiet
.\MaxLogic.Tests.exe --run:MaxLogic.RichIniFile.Utf8.Tests.TRichIniUtf8Tests --exitbehavior:Continue --consolemode:Quiet
```
