@echo off
set rsvars="d:\Program Files (x86)\Embarcadero\Studio\19.0\bin\rsvars.bat"
call %rsvars%
::msbuild DataGrabber.dproj /t:make /p:config=Debug /p:platform=Win32
msbuild DataGrabber.dproj /t:make /p:config=Release /p:platform=Win32
pause
