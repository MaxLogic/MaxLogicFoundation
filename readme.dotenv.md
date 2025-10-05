# maxlogic.DotEnv

`maxlogic.DotEnv` is a dependency-light Delphi 11+ unit that loads `.env` style configuration files with modern conveniences such as layered configuration, variable interpolation, expression evaluation, and guarded command substitution. It runs cross-platform (Windows, Linux, macOS) and avoids third-party dependencies, making it suitable for console services, GUI apps, and servers alike.

## Highlights

- **Layered loading**: automatically merges `.env`, `.env.local`, and `.env.secret` across user-specified search roots (project folder, parents, home/XDG/APPDATA, custom roots) with deterministic precedence (earlier root wins, later file wins inside a root).
- **Includes**: `#include` and `#include_if_exists` directives are resolved depth-first with cycle detection, size limits, and descriptive tracing.
- **Rich values**: supports unquoted, quoted, triple-quoted, and heredoc styles, plus line continuations.
- **Interpolation & defaults**: `${NAME}` with `${NAME:-fallback}` semantics, built-ins like `CUR_FILE`, `CUR_DIR`, `APP_EXE`, `APP_DIR`, and case-sensitive behaviour controlled per platform.
- **Expressions**: `$[ … ]` embeds a small expression engine (arithmetic, boolean, comparisons, function calls such as `join`, `env`, `dirname`, etc.).
- **Optional command substitution**: `$(cmd)` is opt-in via `AllowCommandSubst` and captures trimmed stdout; disabled usage is logged as a warning.
- **Trace & diagnostics**: every considered, loaded, missing, or ignored file is logged for deterministic diagnostics, and structured error records identify parse issues, security violations, and strict-mode failures.

## Quick Start

```pascal
uses
  maxlogic.DotEnv;

procedure Bootstrap;
var
  DotEnv: TDotEnv;
  ApiUrl: string;
begin
  DotEnv := TDotEnv.Create;
  try
    DotEnv.LoadLayered(GetCurrentDir, [SearchParents, SearchUserHome]);

    if DotEnv.TryGetValue('API_URL', ApiUrl) then
      DoSomething(ApiUrl)
    else
      DoSomething('https://localhost:8080');
  finally
    DotEnv.Free;
  end;
end;
```

The snippet loads the layered `.env` set from the current directory, walks up parent folders, and then looks in home directories. Values are exposed via `TryGetValue`, `GetValue`, or `AsDictionary`. `LoadFiles` is available when you need to dictate the file list explicitly.

## Value Syntax Overview

- `KEY=value`: unquoted with optional trailing comment (`#` or `;`) and `\` line continuations.
- `KEY="value"`: supports escapes `\n`, `\r`, `\t`, `\"`, `\\`, `\$`.
- `KEY='value'`: literal, with `''` producing a single quote.
- `KEY="""..."""` or `KEY='''...'''`: triple-quoted multiline literals.
- `KEY<<EOF` / `EOF`: heredoc preserving newline content as-is.

## Interpolation Examples

```
APP_ROOT=${CUR_DIR}
CACHE_DIR=${APP_DIR}/cache
PORT=${PORT:-8080}
API_URL=https://$[ lower(env('HOST')) ].example.com
```

`CUR_DIR` and `CUR_FILE` refer to the file currently being parsed; `APP_EXE` and `APP_DIR` resolve to the running executable and its directory (using `ParamStr(0)` under the hood).

## Expressions Cheat Sheet

```
RESULT=$[ (2 + 2 * 3) ]          # 8
STAGE=$[ upper(env('ENV')) ]     # DEV when ENV=dev
ENABLE=$[ (1 < 2) && ('a' == 'a') ]
CONFIG_PATH=$[ join(APP_DIR, 'config', 'service.json') ]
```

Available functions: `env(name)`, `join(a, b, ...)`, `dirname(path)`, `basename(path)`, `abspath(path)`, `upper(text)`, `lower(text)`, `trim(text)`.

## Includes

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

`#include_if_exists` behaves the same but silently ignores missing files. Cycles are reported as fatal errors with the participating file paths.

## Search Strategy & Options

Use `SetSearchRoots` for bespoke search plans, or rely on the default `LoadLayered` behaviour controlled by options:

- `SearchParents`: walk up to 4 parent directories starting from the base.
- `SearchUserHome`: look in the user’s home (`~/.env*`, `~/.config/maxlogic/`).
- `SearchXDG`: honour the XDG base spec (`$XDG_CONFIG_HOME/maxlogic`, each `$XDG_CONFIG_DIRS/maxlogic`, falling back to `~/.config` and `/etc/xdg`).
- `SearchWindowsProfile`: use `%APPDATA%\MaxLogic`.

The search order is deterministic: earlier roots have higher priority, while inside a root the load order is `.env`, `.env.local`, then `.env.secret`.

## Command Substitution (Opt-In)

```
DotEnv.LoadLayered('.', [AllowCommandSubst]);
USER_NAME=$(whoami)
```

When `AllowCommandSubst` is absent, `$(...)` will be left literal and a warning error record is produced. When enabled, stdout (trimmed) is captured, and non-zero exit statuses become `DekCommand` errors, keeping configuration failures visible.

## Error Handling & Tracing

`TDotEnv` accumulates errors (parse, I/O, strict undefined, security, command) which can be retrieved with `GetErrors`. `GetTrace` returns the ordered list of considered/missing/loaded files—ideal for CI logs. Strict undefined mode (`StrictUndefined`) raises fatal errors for missing interpolation values unless a default is supplied.

## API Reference (Summary)

- `LoadLayered(ABaseDir, Options)`: search + load `.env` layers.
- `LoadFiles(Files, Options)`: load explicit file list in order.
- `SetSearchRoots(Roots)`: override automatic roots.
- `TryGetValue`, `GetValue`, `AsDictionary`, `Expand`.
- `OnResolveSymbol`: hook to resolve custom macros before expression fallback.

Consult the unit for advanced behaviours such as secret-file permission checks and diagnostic structures (`TDotEnvError`).

## Minimal Form Application Example

```pascal
var
  DotEnv: TDotEnv;
  Config: TDictionary<string, string>;

begin
  DotEnv := TDotEnv.Create;
  try
    DotEnv.LoadLayered(ExtractFilePath(ParamStr(0)), [SearchParents, SearchUserHome]);
    Memo1.Lines.Add('Loaded files:');
    Memo1.Lines.AddStrings(DotEnv.GetTrace);

    Config := DotEnv.AsDictionary;
    try
      Memo1.Lines.Add('API_URL=' + Config['API_URL']);
    finally
      Config.Free;
    end;
  finally
    DotEnv.Free;
  end;
end;
```

With this foundation you can stage environment overrides, compose secrets securely, and keep developer, QA, and production configurations in sync without manual rewrites.
