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
  echo "$artist - $album - $title $status"
}

main "$@"
