@echo off
:: Change this path to your project folder inside WSL
set WSL_PROJECT_PATH=/mnt/c/Users/Szabolcs/Desktop/LogFunk/mazerush

:: Run WSL, open Ubuntu, start SWI-Prolog, load main.pl, and run main.
wsl -e bash -ic "cd %WSL_PROJECT_PATH% && swipl -s src/main.pl -g main -t halt"
pause
