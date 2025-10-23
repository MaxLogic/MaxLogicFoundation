# Rich INI Implementation Plan

## Milestone 1 – Core Scaffolding
- Add `MaxLogic.RichIniFile.pas` shell with option structure, defaults, and constructor plumbing (no parsing yet).
- Define internal object model (line kinds, section blocks, key lists, comment entities) plus ownership rules per spec.
- Introduce helper utilities (normalization, comment prefix handling, newline resolution stubs) and unit tests scaffolding.

## Milestone 2 – Parser & Document Loading
- Implement text loading with encoding detection (`eoAutoDetect`, BOM handling) using existing helpers if available.
- Parse source text into ordered line objects, respecting comment ownership modes, missing bracket policy, and duplicate section creation.
- Capture source newline style and raw text segments to support byte-for-byte preservation of untouched lines.
- Add regression fixtures covering comments, blank lines, duplicates, global section, and malformed lines.

## Milestone 3 – Read API Parity
- Implement `ReadString/ReadInteger/ReadBool`, `ReadSection`, `ReadSections`, `ReadSectionValues`, `ReadAllKeyValues`.
- Ensure “last wins” semantics for lookups while enumerations provide collapsed views with stable order.
- Cover case-sensitivity toggle and unnamed section handling in tests.

## Milestone 4 – Write & Delete Operations
- Implement `WriteString` (default + indexed), `AppendKey`, `DeleteKey` overloads, and `EraseSection`.
- Maintain raw text when editing existing lines; generate canonical output for new/modified lines.
- Verify duplicate key behavior, section creation, comment deletion rules, and dirty-state tracking.

## Milestone 5 – Comments & Advanced Utilities
- Implement `ReadComment`, `WriteComment`, `PurgeComments` honoring ownership modes.
- Add tools for multiline values (`WriteMultilineString`, `ReadMultilineString`) with `\n` encoding/decoding.
- Implement consolidation helpers (`ConsolidateSection`, `ConsolidateAll`) collapsing sections/keys per spec.
- Extend tests to cover comment round-tripping, consolidation outcomes, and multiline helpers.

## Milestone 6 – Saving & Persistence
- Implement `SaveToFile` with encoding selection (`SaveEncoding`, `BomPolicy`, `CustomEncoding`) and newline policies.
- Create atomic write workflow (temp file in target directory + rename) with cleanup on failure.
- Verify round-trip integrity (load ⇒ save with no edits preserves bytes), encoding matrix, newline modes, and dirty flag reset.

## Milestone 7 – Validation & Polish
- Run full regression suite; add any missing edge tests (e.g., key delimiter variations, unparsed lines round-trip).
- Perform code review cleanup: comments for complex logic, ensure compliance with conventions, check for leaks/ownership issues.
- Prepare usage notes or README snippet summarizing new API surface for maintainers.
