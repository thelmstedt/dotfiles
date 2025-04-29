#!/bin/bash
status=$(playerctl status 2>/dev/null)
icon="🎜"


if [ "$status" = "Playing" ]; then
  icon="♫"  # simple music note
elif [ "$status" = "Paused" ]; then
  icon="‖"  # pause symbol
elif [ "$status" = "Stopped" ]; then
  icon="■"  # stop symbol
fi

metadata=$(playerctl metadata --format '{{ artist }} - {{ title }}' 2>/dev/null || echo "")
echo "$icon $metadata"