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

- Loading detects BOM and newline style once, normalizes keys/sections through `TStringComparer`/`TIStringComparer`, and tracks dirty state.
- Writes mutate the in-memory AST and mark affected lines “dirty”; untouched lines are emitted verbatim.
- `SaveToFile` resolves the outgoing encoding/newline according to the option set, writes to a temporary file in the destination folder, then replaces the target atomically.

## Limitations

- Inline (same-line) comments remain unsupported; the parser keeps them as part of the value.
- Multi-line values require the provided helper codecs (`WriteMultilineString` / `ReadMultilineString`).
- Options are immutable after construction; create a fresh instance if you need different comparers or comment behavior.

## Testing & benchmarks

- Unit tests: `tests/unit/MaxLogic.RichIniFiles.Tests.pas` (registered in `tests/MaxLogic.Tests.dpr`). Run them via `./tests/build-tests.sh` followed by `./tests/MaxLogic.Tests.exe`.
- Benchmarks: `benchmarks/MaxLogic.RichIniBenchmark.dpr` compares `TRichIniFile`, `TMemIniFile`, and `TIniFile` across load/read/write/save workloads. Build with `./tests/build-delphi.sh ../benchmarks/MaxLogic.RichIniBenchmark.dproj` and run the produced executable to inspect throughput.

## When to choose TRichIniFile

Pick `TRichIniFile` when you need any of:
- Preservation of layout and comments for round-tripping user-edited INI files.
- Duplicate section/key awareness beyond “last wins”.
- Strong control over encoding/newline policy during save.
- Comment ownership that survives edits without manual bookkeeping.
- Atomic saves to avoid partial writes or file corruption on failure.

Stick to `TIniFile` or `TMemIniFile` when you only need quick key/value access and do not care about formatting or duplicates.
