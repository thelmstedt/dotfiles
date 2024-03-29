SPEAKERS="alsa_output.usb-SMSL_AUDIO_SMSL_AD18_AMP-00.iec958-stereo"
HEADPHONES="alsa_output.usb-Grace_Design_SDAC-00.iec958-stereo"

CURRENT=$(pactl list short sink-inputs | head -n 1 | awk '{print $2}')
CURRENT_NAME=$(pactl list short sinks | grep "^${CURRENT}[^0-9]" | awk '{print $2}')

IDX="H"
if [[ "$CURRENT_NAME" == "$SPEAKERS" ]]; then
  IDX="S"
fi

echo $IDX