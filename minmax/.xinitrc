xmodmap ~/.xmodmap

xrdb -merge ~/.Xresources

xsetroot -cursor_name left_ptr
xset m 3/2 0 # mouse acceleration

# ghetto lockscreen
xautolock -time 10 -locker slock &

dropbox &
slack &
pidgin &

eval $(ssh-agent)

export _JAVA_AWT_WM_NONREPARENTING=1

exec xmonad
