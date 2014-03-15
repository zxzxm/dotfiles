# -*- mode: Tcl -*-
# Connect to Zone4

#set font -monotype-*-*-*-*-*-10-*-*-*-*-*-*-*
#set ::xtermArgs "-geometry 80x30 -font $font"
set ::xtermArgs "-geometry 80x20 -fa \"fixed:antialias=true:rgba=0:pixelsize=10\""

set xtermCmd "/home/thermans/bin/nx-adm"
set xtermNames {rep1 rep2}
xtermStartAll
