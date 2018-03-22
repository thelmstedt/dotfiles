#!/bin/sh

main() {
  if ! pgrep -x spotify >/dev/null; then
    echo ""; exit
  fi

  cmd="org.freedesktop.DBus.Properties.Get"
  domain="org.mpris.MediaPlayer2"
  path="/org/mpris/MediaPlayer2"

  meta=$(dbus-send --print-reply --dest=${domain}.spotify \
    /org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get string:${domain}.Player string:Metadata)

  album=$(echo $meta | sed 's/dict entry/\n/g' | grep '"xesam:album"'  | sed -nr 's/.* variant.* string \"(.*)\".*/\1/p')
  artist=$(echo $meta | sed 's/dict entry/\n/g' | grep '"xesam:albumArtist"'  | sed -nr 's/.* variant.* string \"(.*)\".*/\1/p')
  title=$(echo $meta | sed 's/dict entry/\n/g' | grep '"xesam:title"'  | sed -nr 's/.* variant.* string \"(.*)\".*/\1/p')

  echo "$artist - $album - $title"
}

main "$@"
