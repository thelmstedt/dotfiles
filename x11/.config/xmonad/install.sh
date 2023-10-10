
# cabal update
cabal install --package-env=$HOME/.config/xmonad --lib base xmonad xmonad-contrib xmonad-dbus X11
cabal install --package-env=$HOME/.config/xmonad xmonad xmonad-dbus
xmonad --recompile