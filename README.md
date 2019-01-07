
# Requirements:

## Arch

* yay


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
* freetype2
* adobe-source-code-pro-fonts


# Hacks:


## Audio bullshit

use `pavucontrol` to view/modify output sources

# Services

## ssh-agent

	systemctl --user enable ssh-agent
	systemctl --user start ssh-agent


# App Setup

## Firefox

https://github.com/yourduskquibbles/webannoyances

### Tree Style Tabs

link `./extrabits/firefox/userChrome.css` to `~/.mozilla/firefox/PROFILE/chrome/userChrome.css`

For tree style tabs, everything must open as a nested tab, no new windows for target=_blank
    
    browser.link.open_newwindow.restriction=0



## IntelliJ 


### Focus follows mouse

[Fix described here](https://youtrack.jetbrains.com/issue/IDEA-112015#comment=27-1324403)

In the registry  (Ctrl+Shift+A and look for "Registry...")

 - disable `allow.dialog.based.popup`
 - enable `focus.follows.mouse.workarounds`