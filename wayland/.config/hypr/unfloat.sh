#!/bin/bash

FLOATING=$(hyprctl activewindow -j | jq '.floating')

if [ "$FLOATING" = "true" ]; then
    hyprctl dispatch togglefloating active
    hyprctl dispatch resetsize
fi

hyprctl dispatch splitratio exact 0.5