@echo off
brcc32 xcalGregorian.rc
brcc32 xcalPersian.rc
brcc32 xcalHijri.rc
move /Y *.RES ..\..\Source
pause