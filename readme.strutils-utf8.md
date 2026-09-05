# Native UTF-8 filtering and comparison

Use `TFilterExUtf8` when our input is already `UTF8String`. It parses once and retains UTF-8 terms. `Matches`, the UTF8String mask overloads, and the UTF-8 equality/hash methods allocate no temporary strings or heap blocks on successful calls. They decode individual scalar values directly from bytes; they never convert whole inputs to `UnicodeString`.

```pascal
var
  lFilter: TFilterExUtf8;
  lText: UTF8String;
  lComparer: IEqualityComparer<UTF8String>;
begin
  lFilter := TFilterExUtf8.Create('invoice !draft *.pdf');
  lText := 'invoice-2026.pdf';
  Assert(lFilter.Matches(lText));
  lComparer := TFastCaseAwareComparer.OrdinalIgnoreCaseUtf8;
  Assert(lComparer.Equals(lText, UTF8String('INVOICE-2026.PDF')));
end;
```

When constructing test/input text from a single non-ASCII character, use `UTF8String(string(c))` or `UTF8Encode`, not `UTF8String(c)`: Delphi can treat the latter as a one-byte character cast rather than UTF-8 encoding.

Pass `TFastCaseAwareComparer.OrdinalUtf8` or `.OrdinalIgnoreCaseUtf8` to `TDictionary<UTF8String, TValue>.Create`. The factories return shared, immutable comparer instances. The original `TFilterEx` and Unicode comparer APIs retain their existing behavior.

## Filter syntax

- Plain terms are case-insensitive substrings; separate terms with ASCII whitespace for AND.
- `a|b` requires either alternative. `!term` negates a term or OR group.
- Quotes retain whitespace or `|` within a term; doubled quotes inside quotes represent a literal quote.
- `prefix*`, `*suffix` and `*substring*` use dedicated matching paths.
- Masks support `*`, `?`, `[abc]`, `[!abc]` and `[a-z]`. Masks match the whole input.
- An empty/default filter accepts every input. As in `TFilterEx`, `**` becomes an empty contains term and does not match.

`StringMatches(UTF8String, UTF8String, aCaseSensitive)` uses the native mask engine. `MatchesFilter(UTF8String, TArray<UTF8String>)` checks case-insensitive masks; an empty array accepts every input. Invalid or descending character-set ranges raise `EMaskException`. Prepared filters validate masks during creation.

## Text semantics

- Case-insensitive matching uses the same simple BMP uppercase mapping as `TFastCaseAwareComparer`, through the installed Delphi RTL. There are no expansions such as sharp-s to `SS`, normalization, or locale-sensitive collation. Supplementary characters remain case-sensitive, matching the existing comparer contract.
- `?` consumes one Unicode scalar, so one `?` matches an emoji. Embedded NULs and line breaks are ordinary characters.
- Malformed UTF-8 consumes one offending byte at a time. Such bytes remain distinct opaque values, rather than becoming replacement characters. Ordinal comparison always compares the original bytes.
- Native suffix comparisons and character-set masks therefore have an explicit ordinal contract. They do not reproduce incidental locale or UTF-16-code-unit behavior of `System.Masks`, `StartsStr`, or `EndsStr`.
- Comparer hash values are internal and must not be persisted as a stable format.

## Verification and measurement

`tests/unit/MaxLogic.StrUtils.Tests.pas` includes real-allocator instrumentation asserting zero allocations for warmed native filtering, masks, equality and hashing. It also covers all non-surrogate BMP case mappings, UTF-8 boundary sequences, malformed bytes, wildcard backtracking, quotes, negation, OR groups and copied filter records.

Build with `tests\build-delphi.bat MaxLogic.Tests.dproj -config Release`, then run `tests\MaxLogic.Tests.exe --run:MaxLogic.StrUtils.Tests.TMaxLogicStrUtilsTests --exitbehavior:Continue --consolemode:Quiet`. `Utf8_Performance` reports nine warmed batches (median, p95, minimum and maximum), each containing 12,000 filter-and-hash operations across short, long and rejected UTF-8 text. Setup is outside timing; timings are reported without a flaky speed threshold.

Measured on this Windows machine with Delphi 12 / Win32 Release (2026-09-05):

| Filter + hash batch | Median | p95 | Minimum |
| --- | ---: | ---: | ---: |
| Conversion-based baseline | 8.042 ms | 8.451 ms | 7.812 ms |
| Native UTF-8 | 6.894 ms | 8.543 ms | 6.862 ms |

Median elapsed time fell by 14.3%; p95 was essentially unchanged. This is a bounded mixed-input microbenchmark, not a speed guarantee for every filter shape. The allocation regression changed from 17 allocations to zero for the warmed native operation.
