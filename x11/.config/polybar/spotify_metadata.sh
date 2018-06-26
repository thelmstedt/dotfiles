#!/bin/bash

main() {
  if ! pgrep -x spotify >/dev/null; then
    echo ""; exit
  fi

  meta=$(dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get string:org.mpris.MediaPlayer2.Player string:Metadata)

  album=$(echo $meta | sed 's/dict entry/\n/g' | grep '"xesam:album"'  | sed -nr 's/.* variant.* string \"(.*)\".*/\1/p')
  artist=$(echo $meta | sed 's/dict entry/\n/g' | grep '"xesam:albumArtist"'  | sed -nr 's/.* variant.* string \"(.*)\".*/\1/p')
  title=$(echo $meta | sed 's/dict entry/\n/g' | grep '"xesam:title"'  | sed -nr 's/.* variant.* string \"(.*)\".*/\1/p')
  playing=$(dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get string:'org.mpris.MediaPlayer2.Player' string:'PlaybackStatus')
  status="" #paused
  if [[ $playing =~ "Playing" ]]
  then
    status="" #play
  fi
  AAT="$artist - $album - $title $status"
  AT="$artist - $title $status"
  T="$title $status"
  MAX=80
  # Only 80 characters, eliding if necessary
  OUT=$AAT
  [ "${#OUT}" -gt $MAX ] && OUT=$AT
  [ "${#OUT}" -gt $MAX ] && OUT=$T
  [ "${#OUT}" -gt $MAX ] && echo $(echo $OUT | cut -c 1-77)... || echo $OUT
}

main "$@"
