# Packages
AddPackage eog # Eye of Gnome
AddPackage evince # Document viewer (PDF, Postscript, djvu, tiff, dvi, XPS, SyncTex support with gedit, comics books (cbr,cbz,cb7 and cbt))
AddPackage file-roller # Create and modify archives
AddPackage gnome-calculator # GNOME Scientific calculator
AddPackage gnome-keyring # Stores passwords and encryption keys
AddPackage hardinfo # A system information and benchmark tool.
AddPackage haskell-dbus # A client library for the D-Bus IPC system
AddPackage luit # Filter that can be run between an arbitrary application and a UTF-8 terminal emulator
AddPackage mplayer # Media player for Linux
AddPackage network-manager-applet # Applet for managing network connections
AddPackage networkmanager-openconnect # NetworkManager VPN plugin for OpenConnect
AddPackage notify-osd # Canonical's on-screen-display notification agent, implementing the freedesktop.org Desktop Notifications Specification with semi-transparent click-through bubbles
AddPackage noto-fonts # Google Noto TTF fonts
AddPackage noto-fonts-cjk # Google Noto CJK fonts
AddPackage noto-fonts-emoji # Google Noto emoji fonts
AddPackage noto-fonts-extra # Google Noto TTF fonts - additional variants
AddPackage openconnect # Open client for Cisco AnyConnect VPN
AddPackage psensor # A graphical hardware temperature monitor for Linux
AddPackage rofi # A window switcher, application launcher and dmenu replacement
AddPackage seahorse # GNOME application for managing PGP keys.
AddPackage slock # A simple screen locker for X
AddPackage thunar # Modern file manager for Xfce
AddPackage thunar-archive-plugin # Create and extract archives in Thunar
AddPackage thunar-media-tags-plugin # Adds special features for media files to the Thunar File Manager
AddPackage thunar-volman # Automatic management of removeable devices in Thunar
AddPackage ttf-dejavu # Font family based on the Bitstream Vera Fonts with a wider range of characters
AddPackage ttf-font-awesome # Iconic font designed for Bootstrap
AddPackage xdo # Utility for performing actions on windows in X
AddPackage xdotool # Command-line X11 automation tool
AddPackage xf86-video-vesa # X.org vesa video driver
AddPackage xmonad # Lightweight X11 tiled window manager written in Haskell
AddPackage xmonad-contrib # Add-ons for xmonad
AddPackage xorg-bdftopcf # Convert X font from Bitmap Distribution Format to Portable Compiled Format
AddPackage xorg-docs # X.org documentations
AddPackage xorg-font-util # X.Org font utilities
AddPackage xorg-fonts-100dpi # X.org 100dpi fonts
AddPackage xorg-fonts-75dpi # X.org 75dpi fonts
AddPackage xorg-fonts-encodings # X.org font encoding files
AddPackage xorg-iceauth # ICE authority file utility
AddPackage xorg-mkfontscale # Create an index of scalable font files for X
AddPackage xorg-server # Xorg X server
AddPackage xorg-server-common # Xorg server common files
AddPackage xorg-server-devel # Development files for the X.Org X server
AddPackage xorg-server-xephyr # A nested X server that runs as an X application
AddPackage xorg-server-xnest # A nested X server that runs as an X application
AddPackage xorg-server-xvfb # Virtual framebuffer X server
AddPackage xorg-sessreg # Register X sessions in system utmp/utmpx databases
AddPackage xorg-setxkbmap # Set the keyboard using the X Keyboard Extension
AddPackage xorg-smproxy # Allows X applications that do not support X11R6 session management to participate in an X11R6 session
AddPackage xorg-x11perf # Simple X server performance benchmarker
AddPackage xorg-xauth # X.Org authorization settings program
AddPackage xorg-xbacklight # RandR-based backlight control application
AddPackage xorg-xcmsdb # Device Color Characterization utility for X Color Management System
AddPackage xorg-xcursorgen # Create an X cursor file from PNG images
AddPackage xorg-xdpyinfo # Display information utility for X
AddPackage xorg-xdriinfo # Query configuration information of DRI drivers
AddPackage xorg-xev # Print contents of X events
AddPackage xorg-xgamma # Alter a monitor's gamma correction
AddPackage xorg-xhost # Server access control program for X
AddPackage xorg-xinit # X.Org initialisation program
AddPackage xorg-xinput # Small commandline tool to configure devices
AddPackage xorg-xkbcomp # X Keyboard description compiler
AddPackage xorg-xkbevd # XKB event daemon
AddPackage xorg-xkbutils # XKB utility demos
AddPackage xorg-xkill # Kill a client by its X resource
AddPackage xorg-xlsatoms # List interned atoms defined on server
AddPackage xorg-xlsclients # List client applications running on a display
AddPackage xorg-xmodmap # Utility for modifying keymaps and button mappings
AddPackage xorg-xpr # Print an X window dump from xwd
AddPackage xorg-xprop # Property displayer for X
AddPackage xorg-xrandr # Primitive command line interface to RandR extension
AddPackage xorg-xrdb # X server resource database utility
AddPackage xorg-xrefresh # Refresh all or part of an X screen
AddPackage xorg-xset # User preference utility for X
AddPackage xorg-xsetroot # Classic X utility to set your root window background to a given pattern or color
AddPackage xorg-xvinfo # Prints out the capabilities of any video adaptors associated with the display that are accessible through the X-Video extension
AddPackage xorg-xwd # X Window System image dumping utility
AddPackage xorg-xwininfo # Command-line utility to print information about windows on an X server
AddPackage xorg-xwud # X Window System image undumping utility


# AUR
AddPackage --foreign polybar # A fast and easy-to-use status bar
AddPackage --foreign xbanish # Hide the mouse cursor when typing
AddPackage --foreign xlayoutdisplay # Detects and arranges linux display outputs, using XRandR for detection and xrandr for arrangement.
AddPackage --foreign xmonad-log # DBus monitor for xmonad log events

# Files
CopyFile /etc/X11/xorg.conf

# Services
CreateLink /etc/systemd/system/multi-user.target.wants/NetworkManager.service /usr/lib/systemd/system/NetworkManager.service
CreateLink /etc/systemd/system/dbus-org.freedesktop.nm-dispatcher.service /usr/lib/systemd/system/NetworkManager-dispatcher.service
CreateLink /etc/systemd/system/network-online.target.wants/NetworkManager-wait-online.service /usr/lib/systemd/system/NetworkManager-wait-online.service
CreateLink /etc/systemd/user/sockets.target.wants/gpg-agent-browser.socket /usr/lib/systemd/user/gpg-agent-browser.socket
CreateLink /etc/systemd/user/sockets.target.wants/gpg-agent-extra.socket /usr/lib/systemd/user/gpg-agent-extra.socket
CreateLink /etc/systemd/user/sockets.target.wants/gpg-agent-ssh.socket /usr/lib/systemd/user/gpg-agent-ssh.socket
CreateLink /etc/systemd/user/sockets.target.wants/gpg-agent.socket /usr/lib/systemd/user/gpg-agent.socket
CreateLink /etc/systemd/user/sockets.target.wants/p11-kit-server.socket /usr/lib/systemd/user/p11-kit-server.socket
