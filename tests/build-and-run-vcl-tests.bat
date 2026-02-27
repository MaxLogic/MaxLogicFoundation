@echo off
setlocal

pushd "%~dp0"

if exist dunitx-vcl-results.xml del /q dunitx-vcl-results.xml >nul 2>&1

call build-delphi.bat MaxLogic.Vcl.Tests.dproj -config Debug
if errorlevel 1 (
  set "EXITCODE=%ERRORLEVEL%"
  goto done
)

call MaxLogic.Vcl.Tests.exe
if not exist dunitx-vcl-results.xml (
  echo ERROR: VCL tests did not produce dunitx-vcl-results.xml
  set "EXITCODE=1"
  goto done
)

findstr /I /C:"success=\"False\"" dunitx-vcl-results.xml >nul
if not errorlevel 1 (
  echo ERROR: VCL tests reported failures. See dunitx-vcl-results.xml
  set "EXITCODE=1"
  goto done
)

set "EXITCODE=%ERRORLEVEL%"

:done
popd
exit /b %EXITCODE%
