#!/bin/sh

if lsusb | grep -qE '04d9:4545|046d:c52b'; then
  echo '{"text": "", "class": "connected", "tooltip": "kvm: local"}'
  exit
fi

cat <<EOF
{"text": "<span background='red' color='#1a1b26'>LAP</span>", "class": "disconnected", "tooltip": "kvm: remote"}
EOF