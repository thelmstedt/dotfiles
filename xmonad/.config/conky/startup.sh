#!/usr/bin/env sh

killall conky
killall trayer

IFACE=$(awk 'NR>1{print $6}' < /proc/net/arp | head -n1)
cat /home/tim/.config/conky/conkydesktop | sed "s/NETWORK_INTERFACE/$IFACE/g" > ~/.cache/conkydesktop
cat /home/tim/.config/conky/conkytop | sed "s/NETWORK_INTERFACE/$IFACE/g" > ~/.cache/conkytop
conky -c ~/.cache/conkydesktop &
conky -c ~/.cache/conkytop &

trayer --edge top --align left --SetDockType true --SetPartialStrut true --expand false --width 200 --widthtype pixel \
    --transparent true --tint 0x000000 --alpha 0 --height 24  --margin 1720 &
