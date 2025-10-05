@echo off
chcp 65001 >nul
setlocal EnableExtensions EnableDelayedExpansion
pushd "%~dp0"
for /f %%t in ('powershell -NoProfile -Command "Get-Date -Format o"') do set "BUILD_START=%%t"

rem =============================================================================
rem v 1.3
rem
rem Usage:
rem   build-delphi.bat <project.dproj | relative\path\to\project.dproj> [options]
rem
rem Required:
rem   <project.dproj>                Path to the .dproj (absolute or relative to this script)
rem
rem Options:
rem   -ver <N>                       Delphi major version (e.g., 23 for Delphi 12 Athens). Default: 23
rem   -keep-logs                     Keep generated log files (on success AND failure)
rem   -show-warnings-on-success      On success, show warnings/hints in output (hidden by default)
rem
rem Behavior:
rem   • Logs are created next to this script and deleted at the end unless -keep-logs is used.
rem   • On success, warnings/hints are hidden unless -show-warnings-on-success is used.
rem   • On failure, errors are printed with repo path sanitized, THEN cleanup runs.
rem =============================================================================

rem ---------------------------------------------------------------------------
rem CONFIG
rem ---------------------------------------------------------------------------
set "DEFAULT_VER=23"  rem 23 = Delphi 12 Athens
set "ROOT=%CD%"       rem repo root (folder with build.bat)
set "EXITCODE=0"

set "PROJECT="
set "VER=%DEFAULT_VER%"

rem ---------------------------------------------------------------------------
rem Parse args (first non-flag token = project path)
rem ---------------------------------------------------------------------------
:parse_args
if "%~1"=="" goto args_done

rem flags with value
if /I "%~1"=="-ver" (
  if "%~2"=="" ( echo ERROR: -ver requires a value.& set "EXITCODE=2" & goto usage_fail )
  set "VER=%~2"
  shift & shift
  goto parse_args
)

rem flags without value
if /I "%~1"=="-keep-logs" set "KEEP_LOGS=1" & shift & goto parse_args
if /I "%~1"=="-show-warnings-on-success" set "SHOW_WARN_ON_SUCCESS=1" & shift & goto parse_args

rem first non-flag token => project
if not defined PROJECT (
  if exist "%~1" (
    set "PROJECT=%~1"
  ) else if exist "%~dp0%~1" (
    set "PROJECT=%~dp0%~1"
  ) else (
    echo ERROR: Project not found: %~1
    set "EXITCODE=2"
    goto usage_fail
  )
  shift
  goto parse_args
)

rem unknown extra token -> ignore and continue
shift
goto parse_args

:args_done

if not defined PROJECT (
  echo ERROR: No project ^(.dproj^) specified.
  set "EXITCODE=2"
  goto usage_fail
)

set "PRJ_NAME=%PROJECT%"
for %%p in ("%PROJECT%") do set "PROJECT=%%~fp"

echo ========================================
echo == BUILDING %PRJ_NAME% (%PROJECT%)
echo ========================================
echo.

rem ---------------------------------------------------------------------------
rem 1) Resolve Delphi root
rem ---------------------------------------------------------------------------
set "BDS_ROOT="
if exist "C:\Program Files (x86)\Embarcadero\Studio\%VER%.0\bin\rsvars.bat" (
  set "BDS_ROOT=C:\Program Files (x86)\Embarcadero\Studio\%VER%.0"
) else if exist "C:\Program Files (x86)\Embarcadero\RAD Studio\%VER%.0\bin\rsvars.bat" (
  set "BDS_ROOT=C:\Program Files (x86)\Embarcadero\RAD Studio\%VER%.0"
) else (
  call :print_elapsed
  echo FAILED - Delphi %VER%.0 not found
  set "EXITCODE=1"
  goto cleanup
)
call "%BDS_ROOT%\bin\rsvars.bat"

rem ---------------------------------------------------------------------------
rem 2) Locate MSBuild.exe
rem ---------------------------------------------------------------------------
set "MSBUILD="
if exist "%BDS_ROOT%\bin\msbuild.exe" set "MSBUILD=%BDS_ROOT%\bin\msbuild.exe"

if not defined MSBUILD (
  set "VSWHERE=%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe"
  if exist "%VSWHERE%" (
    for /f "usebackq delims=" %%i in (`
      "%VSWHERE%" -latest -requires Microsoft.Component.MSBuild -find "MSBuild\**\Bin\MSBuild.exe"
    `) do if not defined MSBUILD set "MSBUILD=%%i"
  )
)

if not defined MSBUILD if exist "%WINDIR%\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe" set "MSBUILD=%WINDIR%\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe"
if not defined MSBUILD if exist "%WINDIR%\Microsoft.NET\Framework\v4.0.30319\MSBuild.exe"   set "MSBUILD=%WINDIR%\Microsoft.NET\Framework\v4.0.30319\MSBuild.exe"

if not defined MSBUILD (
  call :print_elapsed
  echo FAILED - MSBuild.exe not found
  set "EXITCODE=1"
  goto cleanup
)

rem ---------------------------------------------------------------------------
rem 3) Logs (same dir as this script)
rem ---------------------------------------------------------------------------
for /f %%i in ('powershell -NoProfile -Command "Get-Date -Format yyyyMMdd_HHmmss"') do set "TS=%%i"
set "LOGDIR=%~dp0"
set "FULLLOG=%LOGDIR%build_%TS%.log"
set "OUTLOG=%LOGDIR%out_%TS%.log"
set "ERRLOG=%LOGDIR%errors_%TS%.log"

rem ---------------------------------------------------------------------------
rem 4) Build (all output -> OUTLOG, file loggers -> FULLLOG/ERRLOG)
rem ---------------------------------------------------------------------------
set "ARGS=/p:Config=Release /p:Platform=Win32 /p:DCC_Quiet=true /p:DCC_UseMSBuildExternally=true /p:DCC_UseResponseFile=1 /p:DCC_UseCommandFile=1 /t:Build /nologo /v:m /fl"
set "ARGS=%ARGS% /flp:logfile=%FULLLOG%;verbosity=normal"
set "ARGS=%ARGS% /flp1:logfile=%ERRLOG%;errorsonly;verbosity=quiet"

cmd /c ""%MSBUILD%" "%PROJECT%" %ARGS% > "%OUTLOG%" 2>&1"
set "RC=%ERRORLEVEL%"

rem ---------------------------------------------------------------------------
rem 5) Decide failure (prefer ERRLOG; fallback to scanning OUTLOG)
rem ---------------------------------------------------------------------------
set "ERRCOUNT=0"
set "HAS_ERRORS="

if exist "%ERRLOG%" for %%A in ("%ERRLOG%") do if %%~zA GTR 0 set "HAS_ERRORS=1"

if not defined HAS_ERRORS if not "%RC%"=="0" (
  for /f %%E in ('findstr /I /C:": error " /C:": fatal " "%OUTLOG%" ^| find /c /v ""') do set "ERRCOUNT=%%E"
  if not "!ERRCOUNT!"=="0" set "HAS_ERRORS=1"
)

if defined HAS_ERRORS (
  if exist "%ERRLOG%" (
    if "!ERRCOUNT!"=="0" for /f %%E in ('type "%ERRLOG%" ^| find /c /v ""') do set "ERRCOUNT=%%E"
    echo(
    call :print_elapsed
    echo Build FAILED. Errors: !ERRCOUNT!
    powershell -NoProfile -ExecutionPolicy Bypass -Command ^
      "$root=(Resolve-Path '%ROOT%').Path + '\'; $rx=[regex]::Escape($root);" ^
      "Get-Content -Path '%ERRLOG%' | ForEach-Object {" ^
      "  $s=$_; $s = $s -replace ('(?i)'+$rx),''; $s = $s -replace ('(?i)\['+$rx),'['; $s" ^
      "}"
  ) else (
    echo(
    call :print_elapsed
    echo Build FAILED. No error log generated.
    powershell -NoProfile -ExecutionPolicy Bypass -Command ^
      "$root=(Resolve-Path '%ROOT%').Path + '\'; $rx=[regex]::Escape($root);" ^
      "Get-Content -Path '%OUTLOG%' | ForEach-Object {" ^
      "  $s=$_; $s = $s -replace ('(?i)'+$rx),''; $s = $s -replace ('(?i)\['+$rx),'['; $s" ^
      "}"
  )
  set "EXITCODE=1"
  goto cleanup
)

rem ---------------------------------------------------------------------------
rem 6) Success
rem ---------------------------------------------------------------------------
set "WARNCOUNT=0"
set "HINTCOUNT=0"

for /f %%H in ('
  findstr /I /C:": hint " /C:" hint warning " "%OUTLOG%" ^| find /c /v ""
') do set "HINTCOUNT=%%H"

for /f %%W in ('
  findstr /I /C:": warning " "%OUTLOG%" ^| findstr /I /V /C:" hint warning " ^| find /c /v ""
') do set "WARNCOUNT=%%W"

if defined SHOW_WARN_ON_SUCCESS (
  powershell -NoProfile -ExecutionPolicy Bypass -Command ^
    "$root=(Resolve-Path '%ROOT%').Path + '\'; $rx=[regex]::Escape($root);" ^
    "Get-Content -Path '%OUTLOG%' | ForEach-Object {" ^
    "  $s=$_; $s = $s -replace ('(?i)'+$rx),''; $s = $s -replace ('(?i)\['+$rx),'['; $s" ^
    "}"
  echo(
  call :print_elapsed
  echo SUCCESS. Warnings: !WARNCOUNT!, Hints: !HINTCOUNT!
) else (
  powershell -NoProfile -ExecutionPolicy Bypass -Command ^
    "$root=(Resolve-Path '%ROOT%').Path + '\'; $rx=[regex]::Escape($root);" ^
    "Get-Content -Path '%OUTLOG%' | ForEach-Object {" ^
    "  $s=$_; $s = $s -replace ('(?i)'+$rx),''; $s = $s -replace ('(?i)\['+$rx),'['; " ^
    "  if ($s -notmatch ':\s+warning ' -and $s -notmatch ' hint warning ' -and $s -notmatch ':\s+hint ') { $s }" ^
    "}"
  echo(
  call :print_elapsed
  echo SUCCESS.
)

set "EXITCODE=0"
goto cleanup

:usage_fail
echo.
echo Usage: %~nx0 ^<project.dproj^> [-ver N] [-keep-logs] [-show-warnings-on-success]
goto cleanup

:print_elapsed
for /f %%t in ('powershell -NoProfile -Command "$s=[datetime]::Parse('%BUILD_START%'); ((Get-Date) - $s).ToString('hh\:mm\:ss\.fff')"') do set "ELAPSED=%%t"
echo done in !ELAPSED!
exit /b 0

:cleanup
rem Always run at the end; delete logs unless -keep-logs is set.
if defined KEEP_LOGS (
  echo (Logs kept due to -keep-logs)
) else (
  if defined FULLLOG if exist "%FULLLOG%" del /q "%FULLLOG%" >nul 2>&1
  if defined OUTLOG  if exist "%OUTLOG%"  del /q "%OUTLOG%"  >nul 2>&1
  if defined ERRLOG  if exist "%ERRLOG%"  del /q "%ERRLOG%"  >nul 2>&1
)
set "code=%EXITCODE%"
popd
endlocal & exit /b %code%
