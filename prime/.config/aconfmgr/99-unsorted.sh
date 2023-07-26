
RemoveFile /etc/makepkg.conf
RemoveFile /etc/libvirt/secrets


# Mon May 23 10:14:58 AM AEST 2022 - New / changed files


CopyFile /boot/loader/entries.srel 755
CopyFile /etc/multipath/bindings 600


# Mon May 23 10:14:58 AM AEST 2022 - New file properties


SetFileProperty /etc/multipath mode 700


# Mon May 23 10:14:58 AM AEST 2022 - Extra file properties


SetFileProperty /etc/libvirt/secrets mode ''


# Wed Jul 27 05:17:57 PM AEST 2022 - Unknown packages


AddPackage cuda # NVIDIA's GPU programming toolkit
AddPackage cudnn # NVIDIA CUDA Deep Neural Network library
AddPackage fmt # Open-source formatting library for C++
AddPackage hdf5 # General purpose library and file format for storing scientific data
AddPackage nfs-utils # Support programs for Network File Systems
AddPackage openmpi # High performance message passing library (MPI)
AddPackage rabbitmq # Highly reliable and performant enterprise messaging implementation of AMQP written in Erlang/OTP
AddPackage vtk # Software system for 3D computer graphics, image processing, and visualization


# Wed Jul 27 05:17:57 PM AEST 2022 - Missing packages


RemovePackage pacman
RemovePackage ruby
RemovePackage ruby-bundler


# Wed Jul 27 05:17:57 PM AEST 2022 - Unknown foreign packages


AddPackage --foreign czkawka-gui-bin # Multi functional app to find duplicates, empty folders, similar images etc (GUI)
AddPackage --foreign nvidia-container-toolkit # NVIDIA container runtime toolkit
AddPackage --foreign xmonad-dbus-git # XMonad DBus monitor application and library to easily connect XMonad with Polybar


# Wed Jul 27 05:17:57 PM AEST 2022 - Missing foreign packages


RemovePackage --foreign xmonad-log


# Wed Jul 27 05:17:57 PM AEST 2022 - Extra files


RemoveFile /etc/systemd/user/pipewire.service.wants/pipewire-media-session.service


# Wed Jul 27 05:17:57 PM AEST 2022 - New / changed files


CopyFile /etc/rabbitmq/rabbitmq-env.conf
CreateLink /etc/systemd/system/multi-user.target.wants/rabbitmq.service /usr/lib/systemd/system/rabbitmq.service
CreateLink /etc/systemd/user/pipewire.service.wants/wireplumber.service /usr/lib/systemd/user/wireplumber.service
CreateDir /layer
CopyFile /etc/group
CopyFile /etc/group-
CopyFile /etc/hosts
CopyFile /etc/libvirt/qemu/win10.xml 600
CopyFile /etc/passwd
CopyFile /etc/passwd-
RemoveFile /etc/systemd/user/pipewire-session-manager.service # Replacing symbolic link with symbolic link
CreateLink /etc/systemd/user/pipewire-session-manager.service /usr/lib/systemd/user/wireplumber.service


# Sat Jun 24 09:25:25 AM AEST 2023 - Unknown packages


AddPackage aws-cli-v2 # Unified command line interface for Amazon Web Services (version 2)
AddPackage bazel # Correct, reproducible, and fast builds for everyone
AddPackage crawl-data # Data files for Dungeon Crawl Stone Soup: open-source, single-player, role-playing roguelike game of exploration and treasure-hunting
AddPackage dive # A tool for exploring each layer in a docker image
AddPackage docker-buildx # Docker CLI plugin for extended build capabilities with BuildKit
AddPackage dua-cli # A tool to conveniently learn about the disk usage of directories, fast!
AddPackage eksctl # Command line tool for creating clusters on Amazon EKS
AddPackage gcc12 # The GNU Compiler Collection - C and C++ frontends (12.x.x)
AddPackage gdb # The GNU Debugger
AddPackage grpc-cli # gRPC protocol buffers cli
AddPackage jre17-openjdk # OpenJDK Java 17 full runtime environment
AddPackage keychain # A front-end to ssh-agent, allowing one long-running ssh-agent process per system, rather than per login
AddPackage libnma-gtk4 # NetworkManager GUI client library (GTK4)
AddPackage lzip # A lossless file compressor based on the LZMA algorithm
AddPackage musescore # Create, play and print beautiful sheet music
AddPackage ncftp # A set of free application programs implementing FTP
AddPackage openvpn # An easy-to-use, robust and highly configurable VPN (Virtual Private Network)
AddPackage pacman-contrib # Contributed scripts and tools for pacman systems
AddPackage python-nodeenv # Node.js virtual environment builder
AddPackage rclone # Sync files to and from Google Drive, S3, Swift, Cloudfiles, Dropbox and Google Cloud Storage
AddPackage sdl2_mixer # A simple multi-channel audio mixer (Version 2)
AddPackage sox # The Swiss Army knife of sound processing tools
AddPackage valgrind # Tool to help find memory-management problems in programs


# Sat Jun 24 09:25:26 AM AEST 2023 - Missing packages


RemovePackage cblas
RemovePackage cuda
RemovePackage cudnn
RemovePackage gimp
RemovePackage networkmanager-openconnect
RemovePackage openconnect
RemovePackage python-pytorch
RemovePackage rabbitmq
RemovePackage smem


# Sat Jun 24 09:25:26 AM AEST 2023 - Unknown foreign packages


AddPackage --foreign bunjs-bin # All-in-one JavaScript runtime built for speed, with a native bundler, transpiler, test runner, and npm-compatible package manager baked-in.
AddPackage --foreign ghcup-hs-bin # an installer for the general purpose language Haskell
AddPackage --foreign grpcurl-bin # Like cURL, but for gRPC: Command-line tool for interacting with gRPC servers
AddPackage --foreign networkmanager-openconnect-useragent-git # NetworkManager VPN plugin for OpenConnect with support for custom useragent
AddPackage --foreign openconnect-git # Open client for Cisco AnyConnect VPN
AddPackage --foreign openconnect-sso # Wrapper script for OpenConnect supporting Azure AD (SAMLv2) authentication
AddPackage --foreign plex-desktop # Plex desktop client for linux
AddPackage --foreign protoc-gen-go # Go support for Google's protocol buffers
AddPackage --foreign protoc-gen-go-grpc # gRPC bindings generator for Go language
AddPackage --foreign protoc-gen-grpc-java # Protobuf gRPC compiler for Java.
AddPackage --foreign pyflame # A tool for generating flame graphs for Python (2) processes.
AddPackage --foreign smem # Generate reports on memory usage.
AddPackage --foreign xmonad-log # DBus monitor for xmonad log events


# Sat Jun 24 09:25:26 AM AEST 2023 - Missing foreign packages


RemovePackage --foreign aws-cli-v2-bin
RemovePackage --foreign czkawka-gui-bin
RemovePackage --foreign stone-soup-tiles-git
RemovePackage --foreign vpn-slice
RemovePackage --foreign xmonad-dbus-git


# Sat Jun 24 09:25:26 AM AEST 2023 - Extra files


RemoveFile /etc/systemd/user/sockets.target.wants/gpg-agent-ssh.socket
RemoveFile /etc/systemd/user/sockets.target.wants/gpg-agent.socket
RemoveFile /etc/systemd/user/sockets.target.wants/gpg-agent-extra.socket
RemoveFile /etc/systemd/user/sockets.target.wants/gpg-agent-browser.socket
RemoveFile /etc/systemd/user/sockets.target.wants/dirmngr.socket
RemoveFile /etc/rabbitmq/rabbitmq-env.conf
RemoveFile /etc/rabbitmq
RemoveFile /etc/ImageMagick-7/policy.xml
RemoveFile /etc/ImageMagick-7


# Sat Jun 24 09:25:26 AM AEST 2023 - New / changed files


CopyFile /etc/bash_completion.d/nsdo
CreateDir /etc/credstore 0
CreateDir /etc/credstore.encrypted 0
CopyFile /etc/dnsmasq.conf
CopyFile /etc/libvirt/qemu/win10.xml~ 600
CopyFile /etc/netns/clarivate/resolv.conf
CreateDir /etc/openvpn/client 750 openvpn network
CreateDir /etc/openvpn/server 750 openvpn network
CopyFile /etc/pacman.d/hooks/xmonad-recompile.hook
CopyFile /etc/pacman.d/mirrorlist.backup
CopyFile /etc/security/limits.conf
CopyFile /boot/loader/entries/arch.conf 755
CopyFile /boot/loader/loader.conf 755
CopyFile /etc/group
CopyFile /etc/group-
CopyFile /etc/hosts
CopyFile /etc/libvirt/qemu/win10.xml 600
CopyFile /etc/minio/minio.conf
CopyFile /etc/mkinitcpio.conf
CopyFile /etc/mkinitcpio.d/linux-lts.preset
CopyFile /etc/mkinitcpio.d/linux.preset
CopyFile /etc/modprobe.d/vfio.conf
CopyFile /etc/nginx/sites-enabled/local.tmv.io
CopyFile /etc/pacman.d/mirrorlist
CopyFile /etc/passwd
CopyFile /etc/passwd-
CopyFile /etc/shells


# Sat Jun 24 09:25:26 AM AEST 2023 - New file properties


SetFileProperty /etc/bluetooth mode 755
SetFileProperty /etc/netns group tim
SetFileProperty /etc/netns owner tim
SetFileProperty /etc/tcsd.conf group tss
SetFileProperty /etc/tcsd.conf mode 640


# Sat Jun 24 09:25:26 AM AEST 2023 - Extra file properties


SetFileProperty /etc/minio/minio.conf mode ''
SetFileProperty /etc/redis mode ''
