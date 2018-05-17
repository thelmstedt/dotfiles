#!/usr/bin/env sh

# Terminate already running bar instances
killall -q polybar

# wait until the processes have been shut down
while pgrep -x polybar >/dev/null; do sleep 1; done

# launch polybar
if [ "$(hostname)" = "AM" ]
then
    if type "xrandr"; then
      for m in $(xrandr --query | grep " connected" | cut -d" " -f1); do
        MONITOR=${m} polybar --reload desktop &
      done
    else
      polybar --reload desktop &
    fi

else
  if ! pgrep -x "nm-applet" >/dev/null
  then
    nm-applet &
  fi
  polybar laptop &
fi

echo "Bars launched..."