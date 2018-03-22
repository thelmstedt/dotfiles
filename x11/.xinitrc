xmodmap ~/.xmodmap

xrdb -merge ~/.Xresources

xsetroot -cursor_name left_ptr
xset m 1 0 # mouse acceleration
#xset m 4/3 0 # example speeding up

xautolock -time 10 -locker slock & # ghetto lockscreen
xbanish & # hides mouse cursor when you start typing

dropbox &
slack &
hipchat &
pidgin &
volwheel &

# cargo culting xmonad/java fixes
export _JAVA_AWT_WM_NONREPARENTING=1

#if [ -f /home/tim/.WORK ]; then
#   xrandr --output HDMI-0 --auto --output DVI-I-1 --right-of HDMI-0
#fi

exec xmonad
