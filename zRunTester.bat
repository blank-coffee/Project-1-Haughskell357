@echo off
echo Building File Organizer and Tester...
stack build

if %errorlevel% neq 0 (
    echo.
    echo Build failed! Check the errors above.
    pause
    exit /b %errorlevel%
)

echo Starting Tester...
stack run tester
pause