# Packages
AddPackage amd-ucode # Microcode update files for AMD CPUs
AddPackage archlinux-keyring # Arch Linux PGP keyring
AddPackage autoconf # A GNU tool for automatically configuring source code
AddPackage automake # A GNU tool for automatically creating Makefiles
AddPackage base # Minimal package set to define a basic Arch Linux installation
AddPackage bind # The ISC DNS Server
AddPackage binutils # A set of programs to assemble and manipulate binary and object files
AddPackage bison # The GNU general-purpose parser generator
AddPackage bluez # Daemons for the bluetooth protocol stack
AddPackage bluez-utils # Development and debugging utilities for the bluetooth protocol stack
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
AddPackage net-tools # Configuration tools for Linux networking
AddPackage nvidia # NVIDIA drivers for linux
AddPackage nvidia-lts # NVIDIA drivers for linux-lts
AddPackage nvidia-settings # Tool for configuring the NVIDIA graphics driver
AddPackage nvme-cli # NVM-Express user space tooling for Linux
AddPackage openssh # Premier connectivity tool for remote login with the SSH protocol
AddPackage pacman # A library-based package manager with dependency support
AddPackage pacutils # Helper tools for libalpm
AddPackage pkgconf # Package compiler and linker metadata toolkit
AddPackage smartmontools # Control and monitor S.M.A.R.T. enabled ATA and SCSI Hard Drives
AddPackage sudo # Give certain users the ability to run some commands as root
AddPackage texinfo # GNU documentation system for on-line information and printed output
AddPackage usbutils # A collection of USB tools to query connected USB devices
AddPackage words # A collection of International 'words' files for /usr/share/dict.

# AUR
AddPackage --foreign aconfmgr-git # A configuration manager for Arch Linux
AddPackage --foreign systemd-boot-pacman-hook # Pacman hook to upgrade systemd-boot after systemd upgrade.
AddPackage --foreign yay-bin # Yet another yogurt. Pacman wrapper and AUR helper written in go. Pre-compiled.

# Files

CopyFile /.BUILDINFO
CopyFile /.MTREE
CopyFile /.PKGINFO
CopyFile /boot/loader/entries/arch-lts.conf 755
CopyFile /boot/loader/entries/arch.conf 755
CopyFile /boot/loader/loader.conf 755
CopyFile /boot/syslinux/syslinux.cfg 755
CopyFile /etc/adjtime
CopyFile /etc/conf.d/lm_sensors
CopyFile /etc/fstab
CopyFile /etc/group
CopyFile /etc/group-
CopyFile /etc/hostname
CopyFile /etc/hosts
CopyFile /etc/locale.conf
CopyFile /etc/machine-id 444
CopyFile /etc/makepkg.conf
CopyFile /etc/mkinitcpio.conf
CopyFile /etc/mkinitcpio.d/linux-lts.preset
CopyFile /etc/mkinitcpio.d/linux.preset
CopyFile /etc/modules-load.d/nct6775.conf
CopyFile /etc/nvme/hostid
CopyFile /etc/nvme/hostnqn
CopyFile /etc/pacman.conf
CopyFile /etc/pacman.d/mirrorlist
CopyFile /etc/passwd
CopyFile /etc/passwd-
CopyFile /etc/resolv.conf.head
CopyFile /etc/shells
CopyFile /etc/sudoers
CopyFile /etc/sysctl.d/99-sysctl.conf
CopyFile /etc/systemd/homed.conf
CreateFile /etc/pacman.d/gnupg/.gpg-v21-migrated > /dev/null
CreateLink /etc/localtime /usr/share/zoneinfo/Australia/Brisbane
CreateLink /etc/os-release ../usr/lib/os-release


# Services
CreateLink /etc/systemd/system/bluetooth.target.wants/bluetooth.service /usr/lib/systemd/system/bluetooth.service
CreateLink /etc/systemd/system/dbus-org.bluez.service /usr/lib/systemd/system/bluetooth.service
CreateLink /etc/systemd/system/dbus-org.freedesktop.resolve1.service /usr/lib/systemd/system/systemd-resolved.service
CreateLink /etc/systemd/system/dbus-org.freedesktop.timesync1.service /usr/lib/systemd/system/systemd-timesyncd.service
CreateLink /etc/systemd/system/getty.target.wants/getty@tty1.service /usr/lib/systemd/system/getty@.service
CreateLink /etc/systemd/system/multi-user.target.wants/dhcpcd.service /usr/lib/systemd/system/dhcpcd.service
CreateLink /etc/systemd/system/multi-user.target.wants/lm_sensors.service /usr/lib/systemd/system/lm_sensors.service
CreateLink /etc/systemd/system/multi-user.target.wants/remote-fs.target /usr/lib/systemd/system/remote-fs.target
CreateLink /etc/systemd/system/multi-user.target.wants/systemd-resolved.service /usr/lib/systemd/system/systemd-resolved.service
CreateLink /etc/systemd/system/sysinit.target.wants/systemd-timesyncd.service /usr/lib/systemd/system/systemd-timesyncd.service
CreateLink /etc/systemd/system/timers.target.wants/fstrim.timer /usr/lib/systemd/system/fstrim.timer
CreateLink /etc/systemd/user/sockets.target.wants/dirmngr.socket /usr/lib/systemd/user/dirmngr.socket
CreateLink /etc/systemd/user/sockets.target.wants/gcr-ssh-agent.socket /usr/lib/systemd/user/gcr-ssh-agent.socket
