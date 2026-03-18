#!/bin/bash
STATEFILE=/tmp/playerctl_last

playing=$(playerctl -l | xargs -I{} sh -c 'playerctl -p {} status 2>/dev/null | grep -q Playing && echo {}' | head -1)

if [ -n "$playing" ]; then
  echo "$playing" > $STATEFILE
  playerctl --all-players pause
else
  last=$(cat $STATEFILE 2>/dev/null)
  [ -n "$last" ] && playerctl -p "$last" play
fi