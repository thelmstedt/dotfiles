xmodmap ~/.xmodmap

xrdb -merge ~/.Xresources

xsetroot -cursor_name left_ptr
#xset m 1 0
xset m 4/3 0 # mouse acceleration

# lockscreen
[ -x "$(command -v xautolock)" ] && xautolock -time 10 -locker slock &

# cursor vanishes when you start typing
[ -x "$(command -v xbanish)" ] && xbanish &
[ -x "$(command -v dropbox)" ] && dropbox &
[ -x "$(command -v pasystray)" ] && pasystray &
[ -x "$(command -v nm-applet)" ] && nm-applet &

# default apps
[ -x "$(command -v teams)" ] && teams &

# cargo culting xmonad/java fixes
export _JAVA_AWT_WM_NONREPARENTING=1

# "emacs" (readline) shortcuts system wide
if [ -f /home/tim/.config/gtk-3.0/settings.ini ]; then
  gsettings set org.gnome.desktop.interface gtk-key-theme "Emacs"
fi

# keyring
if [ -x "$(command -v gnome-keyring-daemon)" ]; then
  dbus-update-activation-environment --systemd DISPLAY
  eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
  export SSH_AUTH_SOCK
fi

## Recompile to catch changes in ghc/haskell-*
xmonad --recompile || (sudo ghc-pkg recache && xmonad --recompile)

exec xmonad

