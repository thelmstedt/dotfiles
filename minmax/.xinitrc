xmodmap ~/.xmodmap

xrdb -merge ~/.Xresources

xsetroot -cursor_name left_ptr

# ghetto lockscreen
xautolock -time 10 -locker slock &

dropbox &

exec xmonad
