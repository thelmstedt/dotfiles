#!/usr/bin/env sh

# Terminate already running bar instances
killall -q polybar

# wait until the processes have been shut down
while pgrep -x polybar >/dev/null; do sleep 1; done

# launch polybar
if [ "$(hostname)" = "AM" ]
then
    NETWORK_IFACE=enp24s0 polybar --reload work_l &
    NETWORK_IFACE=enp24s0 polybar --reload work_r &
else
    NETWORK_IFACE=enp4s0 polybar --reload home_l &
    NETWORK_IFACE=enp4s0 polybar --reload home_r &
fi

echo "Bars launched..."
