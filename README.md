
# Requirements:

Install yay for AUR

    git clone https://aur.archlinux.org/yay-git.git
    makepkg -si

## Packages

TODO - it'd be great to define this in something executable

Base conf for x11 / desktop environment

    yay -S \
    xorg-xinit xmonad xmonad-contrib haskell-dbus xmonad-log rofi polybar ttf-font-awesome notify-osd \
    xbanish slock \
    pavucontrol volwheel \
    alacritty exa fd ripgrep \
    freetype2 adobe-source-code-pro-fonts \
    gnome-keyring seahorse \
    thunar \
    systemd-boot-pacman-hook
    
Tools

    yay -S \
    aws-cli docker jq parallel nmap unzip zip \
    git tig \
    htop iotop lm-sensors nethogs ncdu \  
    emacs libreoffice \
    eog evince gimp imagemagick \
    spotify teams dropbox icaclient
     
Programming
    
    yay -S \
    jdk8-openjdk gradle visualvm jmeter jmeter-plugins-manager \
    nodejs npm yarn \
    rustup \
    nginx postgresql minio \
    postman-bin 


# Setup

## sysctl

Put `./extrabits/99-sysctl.conf` in `/etc/sysctl.d/99-sysctl.conf`

Reload with `sudo sysctl --system`

## fs trim

https://wiki.archlinux.org/index.php/Solid_state_drive#Periodic_TRIM

    systemctl enable fstrim.timer

## ssh-agent

TODO are we using this?

	systemctl --user enable ssh-agent
	systemctl --user start ssh-agent

## fonts

https://old.reddit.com/r/archlinux/comments/5r5ep8/make_your_arch_fonts_beautiful_easily/

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