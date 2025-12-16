# TRichIniFile – Rich INI reader/writer

`TRichIniFile` is a Delphi 12+ drop-in friendly INI engine that keeps the source file intact while exposing a familiar `TIniFile`/`TMemIniFile` surface. It is designed for tooling and configuration scenarios that need comment preservation, duplicate keys, controlled encodings, and atomic saves without sacrificing performance.

## Highlights
- Preserves the original text layout: comments, blank lines, duplicate sections/keys, and key ordering are kept byte-for-byte unless you mutate them.
- Comment ownership modes (`coAttachToNext`, `coAttachToPrev`, `coNone`) let you control where runs of comments travel during edits.
- Duplicate-aware API: append keys, enumerate all duplicate values, ask for the last occurrence index, and consolidate sections when you need a single view.
- Encoding and newline control: auto-detect BOM/encoding on load, force UTF-8/ANSI/custom encodings on save, and override newline style per spec.
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

Use `CreateFromStrings` when you already have the text in memory and want to avoid filesystem IO:

```pascal
Ini := TRichIniFile.CreateFromStrings(SourceLines, Options);
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
