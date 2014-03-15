# -*- mode: Tcl -*-
# Ten windows in Production
#set font -*-profontwindows-*-*-*-*-12-*-*-*-*-*-*-*
#set ::xtermArgs "-geometry 80x10 -font $font"
set ::xtermArgs "-geometry 80x10 -fa \"fixed:antialias=true:rgba=0:pixelsize=10\""

set xtermCmd "/home/thermans/bin/nx-adm"
set xtermNames {rep1 rep2 rep3 rep4 rep5 rep6 rep7 rep8 rep9 rep10}
xtermStartAll
