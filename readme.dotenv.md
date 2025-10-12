# maxlogic.DotEnv

A dependency-light Delphi 12+ unit for reading `.env` files with modern ergonomics:
layered configuration, includes, interpolation with defaults, a small expression engine,
and opt-in command substitution. Cross-platform (Windows, Linux, macOS). No 3rd-party deps.

This README is a practical guide: when to use it, how to use it, what all the options mean,
how values are resolved, and how to troubleshoot common issues.

Contents
- What it’s for and when to use it
- Feature overview
- Quick start
- Concepts and options (layering, search roots, evaluation modes, precedence, casing)
- Value syntax (unquoted/quoted/triple/heredoc, escaping, interpolation, expressions, commands)
- Diagnostics (trace, errors) and security guards
- Recipes and troubleshooting
- API summary

## When to use it

Use maxlogic.DotEnv when you want:
- Deterministic, layered config for console apps, services, and GUI programs.
- A single unit you can drop into Delphi projects (no external packages).
- Modern `.env` features: includes, defaults, simple expressions, and tracing.
- CI-friendly behavior with reproducible precedence and non-interactive diagnostics.

Avoid using it for:
- Very large config blobs (files > 1 MB are rejected by default for safety; you can raise the cap).
- Secrets that cannot be on disk at all (this reads files; for HSM/secure vaults integrate separately).

## Feature overview

- Layered loading: merges `.env`, `.env.local`, `.env.secret` across multiple search roots.
- Includes: `#include` and `#include_if_exists` (depth-first, cycle detection).
- Rich values: unquoted, double/single quotes, triple quotes, heredoc, line continuations.
- Interpolation: `${NAME}` and `${NAME:-fallback}` with built-ins (`CUR_FILE`, `CUR_DIR`, `APP_EXE`, `APP_DIR`).
- Expressions: `$[ ... ]` with arithmetic/boolean/comparisons and functions (`env`, `join`, `dirname`, `basename`, `abspath`, `upper`, `lower`, `trim`).
- Command substitution (opt-in): `$(cmd)` captures stdout (trimmed), with non-zero exit reported.
- Options: control case sensitivity, strict-undefined, streaming evaluation, extra search roots, “don’t override existing env”.
- Diagnostics: per-file trace and structured errors (parse/strict/cycle/I/O/security/command).
- Safety: file size cap, secret file permission checks (POSIX), explicit opt-in for external command execution.

## Requirements
- Delphi 12+ (uses `THashSet<T>` in System.Generics.Collections)

## Quick start

```pascal
uses maxlogic.DotEnv;

procedure Bootstrap;
var
  DotEnv: TDotEnv;
  ApiUrl: string;
begin
  DotEnv := TDotEnv.Create;
  try
    // Default: search current directory + parents (up to 30), merge .env/.env.local/.env.secret
    DotEnv.LoadLayered(GetCurrentDir, [SearchUserHome]); // also look in user home

    if DotEnv.TryGetValue('API_URL', ApiUrl) then
      InitApi(ApiUrl)
    else
      InitApi('https://localhost:8080');
  finally
    DotEnv.Free;
  end;
end;
```

### Streaming evaluation (legacy behavior)

Default evaluation is “final map” (after merge). To evaluate as you read (streaming):

```pascal
DotEnv.LoadLayered(GetCurrentDir, [StreamingEvaluation]);
```

In streaming mode, forward references are empty (or fatal under StrictUndefined). After-merge mode allows forward references to see final winners.

## Concepts and options

### Layering and precedence

- Files per root in this order: `.env` → `.env.local` → `.env.secret` (later wins within a root).
- Roots are searched in a deterministic order; earlier roots have higher priority.
- Default roots: current dir + parents (up to ParentDepth, default 30). Options can add Home/XDG/APPDATA. You can also provide custom roots. To disable parent traversal entirely, construct with `AParentDepth = 0`: `TDotEnv.Create(AMaxFileSize, 0)`.
- Root precedence when combined (highest to lowest):
  - Project directory (BaseDir passed to LoadLayered)
  - Parent directories of BaseDir (nearest → farthest)
  - POSIX XDG home (`$XDG_CONFIG_HOME/maxlogic`), then each entry from `$XDG_CONFIG_DIRS/maxlogic` in order
  - User home (`~`), then `~/.config/maxlogic`
  - Windows profile (`%APPDATA%\MaxLogic`)

### Search roots

- `SearchUserHome`: include `~/.env*` and `~/.config/maxlogic/`.
- `SearchXDG` (POSIX): include `$XDG_CONFIG_HOME/maxlogic` and each dir from `$XDG_CONFIG_DIRS/maxlogic` (fallbacks supported). The order of `$XDG_CONFIG_DIRS` is preserved; earlier entries have higher priority.
- `SearchWindowsProfile` (Windows): include `%APPDATA%\MaxLogic`.
- Custom roots: provide your own via `SetSearchRoots`.

```pascal
var Roots: TArray<TSearchRoot>;
SetLength(Roots, 2);
Roots[0].Kind := srCustom; Roots[0].Path := 'C:\ConfigA';
Roots[1].Kind := srCustom; Roots[1].Path := 'D:\ConfigB';
DotEnv.SetSearchRoots(Roots);
DotEnv.LoadLayered('C:\Project', []); // only your custom roots are used
```

### Evaluation modes

- Default (after merge): Values are expanded after all layers are merged. Derived keys see final winners.
  - Example: `B=${A}` then later `A=base` → B resolves to `base`.
- Streaming (`StreamingEvaluation`): Values are expanded in file order as they are read. No re-evaluation later.
  - Example: with `B=${A}` then `A=base` in the same file, `B` becomes empty unless a default is provided.

### Process environment semantics

- Read-only: the loader does not modify your process environment.
- Precedence (simplified):
  - Stored values (winners) > Process env (non-empty) > Built-ins > `OnResolveSymbol` hook.
- Empty environment variables are treated as “unset”. Provide `${NAME:-fallback}` if you need a value.
- `DoNotOverrideExisting` option:
  - Keys that already exist in the process environment are not stored from `.env`.
  - Derived keys referencing them (e.g., `DER=${K}`) resolve using the live process value.
  - This avoids stomping over values provided by the host process, services, or CI.
  - Presence-based skip: the “existing” check is based on name presence at load start (not value). Even an empty env var prevents storing the key from files; resolution still treats empty env as unset.
  - Snapshot scope: the env‑name presence snapshot is captured once per `TDotEnv` instance (on first use). To reflect later changes to the process environment for this check, create a fresh instance.

### StrictUndefined

- When enabled, unresolved `${NAME}` without a default is a fatal error and the key is not stored.
- The error message suggests a fix: “Use ${NAME:-fallback} to provide a default.”
- Expressions: also applies to identifiers inside `$[ ... ]`. Unresolved names produce “Undefined identifier NAME” and are fatal when StrictUndefined is enabled.

### Case sensitivity

- Default depends on platform:
  - Windows: case-insensitive keys (last assignment wins, single logical key).
  - POSIX: case-sensitive keys (distinct keys by casing).
- Override with `CaseSensitiveKeys` to force case-sensitive behavior everywhere.
- `AsDictionary` preserves the original casing of winners.
- On case-insensitive maps (default Windows), `AsDictionary` contains one entry per logical key (no duplicates by casing).

## Value syntax

- Unquoted: `KEY=value`
  - Trailing inline comments starting with `#` or `;` are supported (escape them with `\#` or `\;` to keep).
  - Trailing spaces/tabs are trimmed. To keep a trailing space, escape it as `\ ` (e.g., `A=foo\ ` becomes `foo `).
  - Continue lines with a trailing backslash: `\` (preserves newline).
  - Recognized escapes: `\n`, `\r`, `\t`, `\$`, `\\`.
  - Windows paths: prefer quotes or double the backslashes to avoid `\t` becoming a tab.
- Double-quoted: `KEY="value"`
  - Escapes: `\n`, `\r`, `\t`, `\"`, `\\`, `\$` (literal dollar).
- Single-quoted: `KEY='value'`
  - Literal; use `''` to embed a single quote.
- Triple-quoted: `KEY="""..."""` or `KEY='''...'''`
  - Multiline literals. If the content begins with a newline token, the initial newline is consumed; otherwise preserved.
- Heredoc: 
  ```
  KEY<<END
  line1
  line2
  END
  ```
  - Content preserved as-is until the marker line.

### Keys and delimiters

- Keys use the regex `[A-Za-z_][A-Za-z0-9_]*`.
- Delimiters: `=` or `:`.
  - Examples:
    - `HOST=localhost`
    - `PORT: 8080`

### Encoding and line endings

- Files are read as UTF-8.
- Line endings are normalized to `\n` (LF) internally, making diagnostics stable across platforms.

### Escaping quick reference

- Unquoted:
  - Escapes: `\n`, `\r`, `\t`, `\$`, `\\`, plus comment escapes `\#` and `\;`.
  - Inline comments with `#` or `;` start only at column 1 or when preceded by space/tab.
- Double-quoted:
  - Escapes: `\n`, `\r`, `\t`, `\"`, `\\`, `\$` (literal dollar).
- Single-quoted:
  - Literal; use `''` to embed a single quote.
- Literal dollar:
  - Use `\$` in quoted/unquoted text, or `$$` anywhere (e.g., `B=$${A}` stores `${A}` literally).
- Unknown escapes:
  - Unquoted: unrecognized escape sequences are preserved literally (e.g., `\x` remains `\x`).
  - Double-quoted: the backslash is removed and the next character is kept (e.g., `"\x"` becomes `"x"`).

### Heredoc semantics

- Heredoc content is expanded like any other value (variables, expressions, commands).
- Marker match is exact (no trimming). The closing line must equal the marker with no leading/trailing spaces.
- For literal `${A}` inside a heredoc, write `$${A}` or escape dollars as needed.

### Interpolation

- Basic: `${NAME}`; with default: `${NAME:-fallback}`.
- Literal dollars:
  - Use `$$` to emit a literal `$`. Example: `B=$${A}` stores `${A}` literally; call `Expand(...)` later to resolve.
  - In double-quoted/unquoted text, `\$` yields a literal `$` that survives expansion.
- Built-ins:
  - `CUR_DIR` (directory of current .env file), `CUR_FILE` (current file path),
  - `APP_EXE` (full path to ParamStr(0)), `APP_DIR` (its directory),
  - `DIR_SEP` (platform directory separator).
- `OnResolveSymbol`:
  - A callback used only if no stored value, env value, or built-in exists.
  - Receives the current source file path as the second argument; use it to make file‑relative decisions.

```pascal
DotEnv.OnResolveSymbol :=
  function(const Name, FilePath: string): string
  begin
    if SameText(Name, 'DEFAULT_REGION') then
      Result := 'eu-central-1'
    else
      Result := '';
  end;
```

### Expressions

- Syntax: `$[ ... ]`. Supports `+ - * / %`, comparisons `== != < <= > >=`, boolean `&& || !`, grouping with `()`.
- Functions:
  - `env(name)`, `join(a,b,...)`, `dirname(path)`, `basename(path)`, `abspath(path)`,
  - `upper(text)`, `lower(text)`, `trim(text)`.

Examples:
```
SUM=$[ 2 + 2 * 3 ]               # 8
HOST=$[ lower(env('HOST')) ]     # lowercased HOST
CONF=$[ join(APP_DIR, 'cfg', 'app.json') ]  # absolute path
```

#### Type semantics
- “+” concatenates if either side is a string; otherwise it adds numbers (invariant culture).
- Boolean conversion: “true/1” → true; “false/0/''” → false; other strings raise an error.
- Division/modulo by zero are fatal parse errors.
- Path functions use platform rules. `join(...)` combines segments and returns an absolute path (`Combine` + `GetFullPath`). Relative paths are resolved against the process current directory; use `${CUR_DIR}` if you need paths relative to the current .env file.

### Expansion rules and order

- One pass per value:
  - Expansion happens when the key is evaluated (streaming or after-merge). There is no automatic re-expansion later, except when you explicitly call `Expand(...)` on a stored literal.
- Defaults are literal:
  - In `${NAME:-fallback}`, `fallback` is inserted as-is and is not recursively expanded. If you need nested behavior, place the construct inside an expression: `X=$[ upper(${NAME:-anon}) ]`.
- Expressions pre-expand inner constructs:
  - Inside `$[ ... ]` we first expand nested `${...}`, `$(...)`, and nested `$[ ... ]`, then parse and evaluate the expression.
- Forward references:
  - Streaming mode: forward refs are empty (or fatal with `StrictUndefined`).
  - After-merge (default): forward refs see the final winners.

### Command substitution (opt-in)

- Disabled by default. Enable with `AllowCommandSubst`.
- Format: `$(command)`; stdout is captured and trimmed. Non-zero exit produces a `DekCommand` error.
- Disabled: When `AllowCommandSubst` is not set, `$(...)` is left literal and a non-fatal `DekCommand` warning is recorded.
- Capture: On Windows, stdout and stderr are merged; on POSIX, only stdout is captured. Non-zero exit yields an empty value and a `DekCommand` error. The trace includes `exec: ...` with the effective command line.
- Windows: uses `%COMSPEC%` (fallback `cmd.exe`) with `/S /C`. Quote paths/args with spaces. `.bat`/`.cmd` files with UTF‑8 BOM are supported via a BOM‑stripped temporary copy.
- Parentheses: nested parentheses are supported; the parser tracks balanced `(` and `)`.

```pascal
DotEnv.LoadFiles([EnvFile], [AllowCommandSubst]);
WriteText(EnvFile, 'OUT=$("C:\\Tools\\hello.bat" "--flag with space")');
```

## Includes

- Directives:
  - `#include "path/to/file.env"` – required include; missing file yields a DekIO error.
  - `#include_if_exists "path/to/file.env"` – optional include; missing file is logged in trace as “missing:” without an error.
- Resolution:
  - Paths are resolved relative to the including file; absolute paths are used as-is.
  - Includes are processed depth-first. Cycles are detected and reported as `DekCycle` fatal errors.
- Example:
  ```
  # main.env
  DATABASE_URL=postgres://...
  #include "secrets.env"

  # secrets.env
  TOKEN<<END
  line1
  line2
  END
  ```

## Diagnostics and security

- Trace:
  - `GetTrace` returns “considered: … / loaded: … / missing: … / ignored: …” entries in order.
  - Message glossary:
    - `considered: <path>` – path was part of search scope.
    - `loaded: <path>` – file parsed and contributed entries.
    - `missing: <path>` – file not found.
    - `ignored: <path>` – file considered but skipped (e.g., permission/security failure).
    - `exec: <cmdline>` – external command executed (when `AllowCommandSubst`).
    - `skip existing env: KEY` – base key not stored due to `DoNotOverrideExisting`.
    - `skip store due to fatal errors: KEY (file:line)` – key’s expansion emitted fatal errors; value not stored.
- Errors:
  - `GetErrors` returns `TDotEnvError[]` with file/line/col/kind/message/fatal.
  - Kinds: Parse, Strict, Cycle, IO, Security, Command, Warning.
  - Variable cycles: If keys reference each other (e.g., `A=${B}`, `B=${A}`), a `DekCycle` error is recorded and neither key is stored.
  - Cycle scope: Cycle detection applies to after-merge evaluation. In streaming mode, cycles do not produce a cycle error; values are computed in order and unresolved refs are empty or fatal under `StrictUndefined`.
  - StrictUndefined shows: `Undefined variable NAME (no default). Use ${NAME:-fallback} to provide a default.`

- Guards:
  - File size cap (default 1 MB) to avoid accidental giant files; configure via `TDotEnv.Create(AMaxFileSize, AParentDepth)`.
  - Secret file permissions (POSIX): `.env.secret` is case-insensitive (e.g., `.env.SeCrEt`) and must be 0600 (owner read/write only).
  - Command substitution is opt-in to prevent accidental external execution.
- Lifecycle:
  - Each call to `LoadFiles` or `LoadLayered` resets the error list and trace; keep a snapshot if you need historical diagnostics.
  - Value map: Loads do not clear previously loaded keys. Subsequent loads merge/override winners but keep keys not redefined. For isolated reloads, create a fresh `TDotEnv` instance.

## Recipes

- Load explicit files (no search):
  ```pascal
  DotEnv.LoadFiles([PathTo('.env'), PathTo('service.env')], []);
  ```
- Respect system-provided values (services/CI):
  ```pascal
  DotEnv.LoadLayered(BaseDir, [DoNotOverrideExisting]);
  ```
- Fail fast on missing variables:
  ```pascal
  DotEnv.LoadLayered(BaseDir, [StrictUndefined]);
  ```
- Case-sensitive keys everywhere (even on Windows):
  ```pascal
  DotEnv.LoadLayered(BaseDir, [CaseSensitiveKeys]);
  ```
- Deferred expansion:
  ```pascal
  // Store literal ${A}
  WriteText(EnvFile, 'A=one'#10'B=$${A}');
  DotEnv.LoadFiles([EnvFile], []);
  Value := DotEnv.Expand(DotEnv.GetValue('B')); // -> 'one'
  ```

## Troubleshooting

- “Undefined variable NAME” under StrictUndefined:
  - Add a default: `${NAME:-fallback}`, or set the variable in a file or the process env (non-empty).
- Forward reference empty in streaming mode:
  - Either use after-merge evaluation (default), provide a default, or reorder your definitions.
- Windows path got mangled (tabs):
  - Use quotes or double the backslashes in unquoted values: `P=C:\\temp`.
- Command didn’t run:
  - Did you pass `AllowCommandSubst`? On Windows, ensure paths/args are quoted correctly.
- Include cycles:
  - The loader reports the cycle with paths; break the cycle or use `#include_if_exists`.
- Secret permissions (POSIX):
  - `.env.secret` must be owner-only (0600). Fix with `chmod 600 .env.secret`.
- Oversize file rejected:
  - Use `TDotEnv.Create(AMaxFileSize, AParentDepth)` to raise the limit if intentional.

## Thread-safety

TDotEnv instances are not thread-safe. Use one instance per thread, or protect access with external synchronization when sharing.

## API summary

- `constructor Create(AMaxFileSize = 1MB; AParentDepth = 30)`
- `procedure LoadLayered(const BaseDir: string; const Options: TDotEnvOptions = [])`
- `procedure LoadFiles(const Files: array of string; const Options: TDotEnvOptions = [])`
- `procedure SetSearchRoots(const Roots: TArray<TSearchRoot>)`
- `class function DefaultSearchRoots(const BaseDir: string; const Options: TDotEnvOptions): TArray<TSearchRoot>`
- `function TryGetValue(const Key: string; out Value: string): Boolean`
- `function GetValue(const Key: string; const DefaultValue: string = ''): string`
- `function AsDictionary: TDictionary<string,string>` (preserves original winner casing)
- `function Expand(const S: string; const SourceFilePath: string = ''): string`
  - When `SourceFilePath` is omitted, `${CUR_DIR}` resolves to the process current directory and `${CUR_FILE}` is empty.
- `property OnResolveSymbol: TOnResolveSymbol`

Option flags (combine as needed):
- `DoNotOverrideExisting`: don’t store keys that already exist in the process env; derived values read from env.
- `CaseSensitiveKeys`: force case-sensitive keys on all platforms.
- `StrictUndefined`: fatal error for unresolved `${NAME}` without default; key not stored. Hint suggests `${NAME:-fallback}`.
- `AllowCommandSubst`: enable `$(...)` command execution.
- `SearchUserHome`: add user home roots.
- `SearchXDG`: add XDG roots (POSIX).
- `SearchWindowsProfile`: add `%APPDATA%\MaxLogic`.
- `StreamingEvaluation`: evaluate in read order; no re-evaluation after overrides.

With this foundation you can stage environment overrides, compose secrets securely, and keep developer, QA, and production configurations in sync without manual rewrites.
