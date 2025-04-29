#!/usr/bin/env bash

FREQ=$(cat /proc/cpuinfo | grep MHz | awk -F':' '{print $2}' | awk -F'.' '{ total += $1; count++} END { print total/count }')
TEMP=$( (sensors 2> /dev/stdout) | grep "Tctl" | awk '{print $2}' | sed 's/+//g')
FANSPEED=$( (sensors 2> /dev/stdout) | grep "fan1" | awk '{print $2}' | sed 's/+//g')
USAGE=$(top -bn1 | grep "Cpu(s)" | sed "s/.*, *\([0-9.]*\)%* id.*/\1/" | awk '{print int(100 - $1)}')

echo $USAGE% ${FREQ}MHz ${TEMP} ${FANSPEED}RPM
