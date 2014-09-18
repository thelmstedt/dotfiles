#!/usr/bin/sh

killall conky 
killall trayer

conky -c /home/tim/.xmonad/conkydesktop &

trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand false --width 200 --widthtype pixel \
    --transparent true --tint 0x000000 --alpha 0 --height 24 &

conky -c /home/tim/.xmonad/conkytop &

setxkbmap -option 'ctrl:nocaps'
