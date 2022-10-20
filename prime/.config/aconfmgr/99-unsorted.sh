
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
