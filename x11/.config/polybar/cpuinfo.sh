#!/usr/bin/env bash

SPEED=$(sensors | grep fan2 | grep -o "[0-9]\{2,4\}")
FREQ=$(cat /proc/cpuinfo | grep MHz | head -n1 | awk -F':' '{print $2}' | awk -F'.' '{print $1}')
TEMP=$(sensors | grep "Package id 0" | awk '{print $4}' | sed 's/+//g')
#USAGE=$(grep 'cpu ' /proc/stat | awk '{usage=($2+$4)*100/($2+$4+$5)} END {print int(usage)}')
USAGE=$(top -bn1 | grep "Cpu(s)" | sed "s/.*, *\([0-9.]*\)%* id.*/\1/" | awk '{print int(100 - $1)}')

echo $USAGE% ${FREQ}MHz ${SPEED}RPM ${TEMP}
