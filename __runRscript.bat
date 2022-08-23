@echo off

REM This program is meant to quickly and noninteractively run Rscript code.
	REM Each batch file is meant to run a single script. (Due to the amount of time each program takes.)
	REM This version runs Rscript.exe and not R.exe, therefore there is not a "program".Rout.

REM EOF stands for End of File.

SETLOCAL
 
set "Rprogram=calculate_seizure_load_COPY.R"
set currentDir=%CD%

C:

if not exist "Program Files" (goto :DIRFAIL) else (cd Program Files)

REM The FOR loop searches for first instance of Rscript.exe in program files and saves its location
for /f "delims=" %%a in ('dir /s /b Rscript.exe') do (
	set "directory=%%a"
	if [directory]==[] (goto :DIRFAIL) else (goto :RUN)
)

:RUN
cd /D %currentDir%
cd ./..
set dataFolder=%CD%

REM the directory below may need to be changed in the case where the Rscript file is moved
cd ./../../../General Data/Rscript

goto :RSCRIPT

echo Something went wrong...
pause
goto :EOF

:RSCRIPT
echo Running Rscript...
echo.
"%directory%" --vanilla %Rprogram% "%dataFolder%"
echo.
echo End of Rscript...
echo.
pause
goto :EOF


:DIRFAIL
echo R.exe was not found in C:\Program Files
pause
goto :EOF
