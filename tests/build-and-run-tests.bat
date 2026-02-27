@echo off
setlocal

pushd "%~dp0"

rem Run both test suites only if each previous step succeeded.
call build-delphi.bat MaxLogic.Tests.dproj -config Debug && ^
call MaxLogic.Tests.exe && ^
call build-and-run-vcl-tests.bat

set "EXITCODE=%ERRORLEVEL%"

popd
exit /b %EXITCODE%
