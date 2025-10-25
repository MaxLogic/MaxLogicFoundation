@echo off
setlocal

pushd "%~dp0"

rem Run tests only if the build succeeded (exit code 0)
call build-delphi.bat MaxLogic.Tests.dproj -config Debug && ^
call MaxLogic.Tests.exe

rem Preserve the exit code from whichever ran last (build or tests)
set "EXITCODE=%ERRORLEVEL%"

popd
exit /b %EXITCODE%
