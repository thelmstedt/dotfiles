#!/bin/bash
iface="${1:?usage: $0 <iface> <rx|tx>}"
dir="${2:?usage: $0 <iface> <rx|tx>}"

[[ "$dir" == "rx" ]] && f=rx_bytes || f=tx_bytes

cache="/tmp/nettraf_${iface}_${dir}"
now=$(< /sys/class/net/$iface/statistics/$f)

if [[ -f "$cache" ]]; then
  prev=$(< "$cache")
  bps=$(( now - prev ))
else
  bps=0
fi

echo "$now" > "$cache"

if (( bps >= 1048576 )); then
  printf "%03dM/s\n" $(( bps / 1048576 ))
elif (( bps >= 1024 )); then
  printf "%03dK/s\n" $(( bps / 1024 ))
else
  printf "%03dB/s\n" "$bps"
fi