# Packages
AddPackage amd-ucode # Microcode update files for AMD CPUs
AddPackage autoconf # A GNU tool for automatically configuring source code
AddPackage automake # A GNU tool for automatically creating Makefiles
AddPackage base # Minimal package set to define a basic Arch Linux installation
AddPackage bind # The ISC DNS Server
AddPackage binutils # A set of programs to assemble and manipulate binary and object files
AddPackage bison # The GNU general-purpose parser generator
AddPackage bzflag # Multiplayer 3D tank battle game
AddPackage dhcpcd # RFC2131 compliant DHCP client daemon
AddPackage fakeroot # Tool for simulating superuser privileges
AddPackage flex # A tool for generating text-scanning programs
AddPackage gettext # GNU internationalization library
AddPackage groff # GNU troff text-formatting system
AddPackage libtool # A generic library support script
AddPackage linux # The Linux kernel and modules
AddPackage linux-firmware # Firmware files for Linux
AddPackage linux-headers # Headers and scripts for building modules for the Linux kernel
AddPackage linux-lts # The LTS Linux kernel and modules
AddPackage linux-lts-headers # Headers and scripts for building modules for the LTS Linux kernel
AddPackage lm_sensors # Collection of user space tools for general SMBus access and hardware monitoring
AddPackage m4 # The GNU macro processor
AddPackage man-db # A utility for reading man pages
AddPackage nvidia # NVIDIA drivers for linux
AddPackage nvidia-lts # NVIDIA drivers for linux-lts
AddPackage nvidia-settings # Tool for configuring the NVIDIA graphics driver
AddPackage openssh # Premier connectivity tool for remote login with the SSH protocol
AddPackage pacman # A library-based package manager with dependency support
AddPackage pkgconf # Package compiler and linker metadata toolkit
AddPackage sudo # Give certain users the ability to run some commands as root
AddPackage texinfo # GNU documentation system for on-line information and printed output

# AUR
AddPackage --foreign aconfmgr-git # A configuration manager for Arch Linux
AddPackage --foreign systemd-boot-pacman-hook # Pacman hook to upgrade systemd-boot after systemd upgrade.
AddPackage --foreign yay # Yet another yogurt. Pacman wrapper and AUR helper written in go.
AddPackage --foreign zenmonitor # Zen monitor is monitoring software for AMD Zen-based CPUs

# Files

CopyFile /boot/loader/entries/arch-lts.conf 755
CopyFile /boot/loader/entries/arch.conf 755
CopyFile /boot/loader/loader.conf 755
CopyFile /etc/adjtime
CopyFile /etc/conf.d/lm_sensors
CopyFile /etc/fstab
CopyFile /etc/group
CopyFile /etc/group
CopyFile /etc/group-
CopyFile /etc/group-
CopyFile /etc/hostname
CopyFile /etc/hosts
CopyFile /etc/locale.conf
CopyFile /etc/machine-id 444
CopyFile /etc/makepkg.conf
CopyFile /etc/mkinitcpio.conf
CopyFile /etc/mkinitcpio.d/linux-lts.preset
CopyFile /etc/mkinitcpio.d/linux.preset
CopyFile /etc/modprobe.d/blacklist.conf
CopyFile /etc/pacman.d/mirrorlist
CopyFile /etc/passwd
CopyFile /etc/passwd
CopyFile /etc/passwd-
CopyFile /etc/passwd-
CopyFile /etc/resolv.conf
CopyFile /etc/shells
CopyFile /etc/sudoers
CopyFile /etc/sysctl.d/99-sysctl.conf
CopyFile /etc/systemd/homed.conf
CreateFile /etc/pacman.d/gnupg/.gpg-v21-migrated > /dev/null
CreateLink /etc/localtime /usr/share/zoneinfo/Australia/Brisbane
CreateLink /etc/os-release ../usr/lib/os-release


# Services
CreateLink /etc/systemd/system/dbus-org.freedesktop.timesync1.service /usr/lib/systemd/system/systemd-timesyncd.service
CreateLink /etc/systemd/system/getty.target.wants/getty@tty1.service /usr/lib/systemd/system/getty@.service
CreateLink /etc/systemd/system/multi-user.target.wants/dhcpcd.service /usr/lib/systemd/system/dhcpcd.service
CreateLink /etc/systemd/system/multi-user.target.wants/lm_sensors.service /usr/lib/systemd/system/lm_sensors.service
CreateLink /etc/systemd/system/multi-user.target.wants/remote-fs.target /usr/lib/systemd/system/remote-fs.target
CreateLink /etc/systemd/system/sysinit.target.wants/systemd-timesyncd.service /usr/lib/systemd/system/systemd-timesyncd.service
CreateLink /etc/systemd/system/timers.target.wants/fstrim.timer /usr/lib/systemd/system/fstrim.timer
CreateLink /etc/systemd/user/sockets.target.wants/dirmngr.socket /usr/lib/systemd/user/dirmngr.socket
