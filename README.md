
# Requirements:

## Arch

* yay
* freetype2
* adobe-source-code-pro-fonts

## X11

* xmonad
* xmonad-contrib
* haskell-dbus
* xmonad-log
* xbanish
* alacritty
* notify-osd 
* volwheel
* rofi
* polybar
* ttf-font-awesome

# Hacks:

## IntelliJ Focus follows mouse

[Fix described here](https://youtrack.jetbrains.com/issue/IDEA-112015#comment=27-1324403)

In the registry  (Ctrl+Shift+A and look for "Registry...")

 - disable `allow.dialog.based.popup`
 - enable `focus.follows.mouse.workarounds`

## Alsa bullshit

use `pavucontrol` to view/modify output sources

# Services

## ssh-agent

	systemctl --user enable ssh-agent
	systemctl --user start ssh-agent


# Random

## Firefox

https://github.com/yourduskquibbles/webannoyances
