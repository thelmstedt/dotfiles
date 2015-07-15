xmodmap ~/.xmodmap

xrdb -merge ~/.Xresources

xsetroot -cursor_name left_ptr

# ghetto lockscreen
xautolock -time 10 -locker slock &

# removes cursor after period of inactivity
unclutter &

exec xmonad
