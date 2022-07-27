#!/usr/bin/env bash

SPEAKERS="alsa_output.usb-SMSL_AUDIO_SMSL_AD18_AMP-00.iec958-stereo"
HEADPHONES="alsa_output.usb-Grace_Design_SDAC-00.iec958-stereo"

CURRENT=$(pactl list short sink-inputs | head -n 1 | awk '{print $2}')
CURRENT_NAME=$(pactl list short sinks | awk "{ if(\$1 == \"$CURRENT\") { print \$2} }")

IDX=$SPEAKERS
if [[ "$CURRENT_NAME" == "$SPEAKERS" ]]; then
  IDX=$HEADPHONES
fi

pactl set-default-sink "$IDX"
for input_idx in $(pactl list short sink-inputs | awk '{print $1}'); do
  pactl move-sink-input "$input_idx" "$IDX"
done
