@echo off
setlocal

set SCRIPT_DIR=%~dp0
pushd "%SCRIPT_DIR%"

set "BUILD_SCRIPT=..\tests\build-delphi.bat"
if not exist "%BUILD_SCRIPT%" (
  echo ERROR: build helper not found: %BUILD_SCRIPT%
  popd
  exit /b 1
)

set "EXITCODE=0"
for %%P in (*.dproj) do (
  call "%BUILD_SCRIPT%" "%%~fP"
  if errorlevel 1 (
    set "EXITCODE=1"
    goto :done
  )
)

:done
popd
exit /b %EXITCODE%

