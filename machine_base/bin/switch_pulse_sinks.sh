#!/usr/bin/env bash

# sometimes they change names
SPEAKERS=$(pactl list short sinks | grep 'SMSL' | awk '{print $2}')
HEADPHONES=$(pactl list short sinks | grep 'SDAC' | awk '{print $2}')

# RUNNING vs SUSPENDED
CURRENT=$(pactl list short sinks | grep 'RUNNING' | awk '{print $2}')

IDX=$SPEAKERS
if [[ "$CURRENT" == "$SPEAKERS" ]]; then
  IDX=$HEADPHONES
fi

pactl set-default-sink "$IDX"
for input_idx in $(pactl list short sink-inputs | awk '{print $1}'); do
  pactl move-sink-input "$input_idx" "$IDX"
done
