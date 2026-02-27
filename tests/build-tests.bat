@echo off
setlocal

set SCRIPT_DIR=%~dp0
set EXITCODE=0
pushd "%SCRIPT_DIR%"

call build-delphi.bat MaxLogic.Tests.dproj
if errorlevel 1 (
  set EXITCODE=%ERRORLEVEL%
  goto done
)

call build-delphi.bat MaxLogic.Vcl.Tests.dproj
set EXITCODE=%ERRORLEVEL%

:done
popd
exit /b %EXITCODE%
