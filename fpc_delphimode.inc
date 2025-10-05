(* ------------------------------------------------------------------------------}
  delphimode.inc — FPC 3.2 tuned for Delphi 12 compatibility                  }
  Usage: place `{$I delphimode.inc}` at the very top of your units/program.
  Version: 1.0
  Version History:
    - 2025-08/pp: initial version
------------------------------------------------------------------------------*)

{$IFDEF FPC}
  { 1) Compiler dialect & Unicode defaults (Delphi 2009+ semantics) }
  {$mode delphiunicode}     { "string" = UnicodeString, Delphi-like syntax } 
  {$H+}                     { long strings on (Ansi/UnicodeString)         }
  
  // FPC 3.2.x has known issues when a source (or include) file begins with a BOM—especially in combination with {$codepage UTF8} directives—resulting in an internal compiler exception.  - so disable that compiler directive here
  // {$codepage UTF8}          { source literals treated as UTF-8              }

  { 2) Bring in Delphi language features explicitly (safe even if already on) }
  {$modeswitch unicodestrings}   { default string = UnicodeString            }
  {$modeswitch advancedrecords}  { records with methods/properties            }
  {$modeswitch typehelpers}      { helpers for types                          }
  {$modeswitch nestedprocvars}   { nested procedural variables                }
  {$modeswitch out}              { Delphi-style 'out' parameters               }
  {$modeswitch DEFAULTPARAMETERS}
  {$modeswitch ALLOWINLINE}    { inline directive support                   }
  {$modeswitch hintdirective}    { deprecated/platform/library hints          }
  {$modeswitch arrayoperators}   { Delphi-compatible array operators          }

  { 3) Scoped enums like modern Delphi (affects only enums declared under it) }
  {$IFNDEF FPC_NO_SCOPEDENUMS}
    {$scopedenums on}
  {$ENDIF}

  { 4) Cross-compiler conditionals most Delphi code expects }
  {$define UNICODE}                  { Delphi 2009+ indicator                  }
  {$define CONDITIONALEXPRESSIONS}   { many libs gate CompilerVersion checks   }
  {$define VER360}                   { Delphi 12 Athens                        }
  {$define DELPHI12}                 { convenience flag                        }

  { 5) Map common Delphi platform symbols you may see in code }
  {$IFDEF UNIX}
    {$define POSIX}
  {$ENDIF}

  {$IFDEF CPUAARCH64}
    {$define CPUARM64}
    {$define CPU64BITS}
  {$ENDIF}
  {$IFDEF CPUARM}
    {$define CPUARM}     { (kept to mirror Delphi usage; FPC already defines) }
    {$define CPU32BITS}
  {$ENDIF}

  // for TProc like methods. enable "is nested" in 3.2.x and `reference to` in 3.3.1+
  {$if (FPC_FULLVERSION >= 30301)}
    {$modeswitch functionreferences}  // for 3.3.x+
  {$else}
    {$modeswitch nestedprocvars}      // for 3.2.x
  {$endif}

{$ENDIF}



