#!/usr/bin/env bash

SPEAKERS="alsa_output.usb-SMSL_AUDIO_SMSL_AD18_AMP-00.analog-stereo"
HEADPHONES="alsa_output.usb-Grace_Design_SDAC-00.analog-stereo"

CURRENT=$(pactl list short sink-inputs | head -n 1 | awk '{print $2}')
CURRENT_NAME=$(pactl list short sinks | grep "^${CURRENT}.*" | awk '{print $2}')

IDX=$SPEAKERS
if [ "$CURRENT_NAME" == "$SPEAKERS" ]; then
  IDX=$HEADPHONES
fi

pacmd set-default-sink "$IDX"
for input_idx in $(pactl list short sink-inputs | awk '{print $1}'); do
   pactl move-sink-input "$input_idx" "$IDX"
done
