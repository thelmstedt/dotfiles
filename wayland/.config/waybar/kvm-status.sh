#!/bin/sh

if lsusb | grep -qE '04d9:4545|046d:c52b'; then
  echo '{"text": "", "class": "connected", "tooltip": "kvm: local"}'
else
  echo '{"text": "", "class": "disconnected", "tooltip": "kvm: remote"}'
fi