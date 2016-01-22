#!/usr/bin/sh

killall conky
killall trayer

conky -c /home/tim/.xmonad/bin/conkydesktop &
conky -c /home/tim/.xmonad/bin/conkytop &

trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand false --width 200 --widthtype pixel \
    --transparent true --tint 0x000000 --alpha 0 --height 24  --margin 1920 &

#setxkbmap -option 'ctrl:nocaps'
