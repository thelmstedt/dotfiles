#!/usr/bin/env bash

FREQ=$(cat /proc/cpuinfo | grep MHz | awk -F':' '{print $2}' | awk -F'.' '{ total += $1; count++} END { print total/count }')
TEMP=$(sensors | grep "Tctl" | awk '{print $2}' | sed 's/+//g')
FANSPEED=$(sensors | grep "fan2" | awk '{print $2}' | sed 's/+//g')
#USAGE=$(grep 'cpu ' /proc/stat | awk '{usage=($2+$4)*100/($2+$4+$5)} END {print int(usage)}')
USAGE=$(top -bn1 | grep "Cpu(s)" | sed "s/.*, *\([0-9.]*\)%* id.*/\1/" | awk '{print int(100 - $1)}')

echo $USAGE% ${FREQ}MHz ${TEMP} ${FANSPEED}RPM
