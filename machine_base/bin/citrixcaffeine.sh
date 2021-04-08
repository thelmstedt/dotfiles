#!/usr/bin/env bash
##
## Shitty caffiene for citrix since I can't install it.
## Sends a (hopefully) non-side-effecting keystroke to any open citrix window every minute
## I just run this in a terminal while I'm using citrix
##
while true
do
  echo .
  for x in `xdotool search -class Wfica`
  do
     echo xdotool key --window $x ctrl+alt+shift+F12
     xdotool key --window $x ctrl+alt+shift+F12 #ideally this does nothing, it just needs to send something
  done
  sleep 60
done