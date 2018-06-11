xmodmap ~/.xmodmap

xrdb -merge ~/.Xresources

xsetroot -cursor_name left_ptr
#xset m 1 0
xset m 4/3 0 # mouse acceleration

[ -x "$(command -v xautolock)" ] && xautolock -time 10 -locker slock &
[ -x "$(command -v xbanish)" ] && xbanish &

[ -x "$(command -v dropbox)" ] && dropbox &
[ -x "$(command -v volwheel)" ] && volwheel &
[ -x "$(command -v slack)" ] && slack &
[ -x "$(command -v pidgin)" ] && pidgin &

# cargo culting xmonad/java fixes
export _JAVA_AWT_WM_NONREPARENTING=1

if [ -f /home/tim/.WORK ]; then
   xrandr --output HDMI-0 --auto --output DVI-I-1 --right-of HDMI-0
fi

## Recompile to catch changes in ghc/haskell-*
xmonad --recompile && exec xmonad
