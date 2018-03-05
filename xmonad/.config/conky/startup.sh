#!/usr/bin/env sh

killall conky
killall trayer

conky -c /home/tim/.config/conky/conkydesktop &
conky -c /home/tim/.config/conky/conkytop &

trayer --edge top --align left --SetDockType true --SetPartialStrut true --expand false --width 200 --widthtype pixel \
    --transparent true --tint 0x000000 --alpha 0 --height 24  --margin 1720 &
