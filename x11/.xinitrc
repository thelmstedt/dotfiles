#!/bin/bash

xmodmap ~/.xmodmap

xrdb -merge ~/.Xresources

xsetroot -cursor_name left_ptr
#xset m 1 0
xset m 4/3 0 # mouse acceleration

# lockscreen
[ -x "$(command -v xautolock)" ] && xautolock -time 10 -locker slock &

# notification daemon
[ -x "$(command -v dunst)" ] && dunst &

# cursor vanishes when you start typing
[ -x "$(command -v xbanish)" ] && xbanish &
# audio
[ -x "$(command -v pasystray)" ] && pasystray &
# networking
[ -x "$(command -v nm-applet)" ] && nm-applet &

# default apps
[ -x "$(command -v dropbox)" ] && dropbox &
[ -x "$(command -v 1password)" ] && 1password &

# cargo culting xmonad/java fixes
export _JAVA_AWT_WM_NONREPARENTING=1

# "emacs" (readline) shortcuts system wide
if [ -f "$HOME/.config/gtk-3.0/settings.ini" ]; then
  gsettings set org.gnome.desktop.interface gtk-key-theme "Emacs"
fi

# keyring
if [ -x "$(command -v gnome-keyring-daemon)" ]; then
  dbus-update-activation-environment --systemd DISPLAY
  eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
  export SSH_AUTH_SOCK
fi

## machine specific
[[ -s "$HOME/.machineconf" ]] && source "$HOME/.machineconf"

exec xmonad-x86_64-linux

