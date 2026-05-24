#!/bin/bash
target=$1
workspace=$(hyprctl activeworkspace -j | jq -r '.id')
fullscreen=$(hyprctl activeworkspace -j | jq -r '.hasfullscreen')
orientation=$(hyprctl getoption master:orientation -j | jq -r '.str')

case $target in
  monocle)
    [[ $fullscreen == "true" ]] && exit
    hyprctl dispatch fullscreen 1 ;;
  tall)
    [[ $fullscreen == "false" && $orientation == "left" ]] && exit
    hyprctl dispatch fullscreen 0
    hyprctl dispatch layoutmsg orientationleft ;;
  wide)
    [[ $fullscreen == "false" && $orientation == "bottom" ]] && exit
    hyprctl dispatch fullscreen 0
    hyprctl dispatch layoutmsg orientationbottom ;;
esac