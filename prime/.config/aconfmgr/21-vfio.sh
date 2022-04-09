# Packages
AddPackage dnsmasq # Lightweight, easy to configure DNS forwarder and DHCP server
AddPackage edk2-ovmf # Open Virtual Machine Firmware to support firmware for Virtual Machines
AddPackage iptables-nft # Linux kernel packet control tool (using nft interface)
AddPackage libvirt # API for controlling virtualization engines (openvz,kvm,qemu,virtualbox,xen,etc)
AddPackage qemu # A generic and open source machine emulator and virtualizer
AddPackage virt-manager # Desktop user interface for managing virtual machines

# AUR

# Files

CopyFile /etc/libvirt/libvirtd.conf
CopyFile /etc/libvirt/nwfilter/allow-arp.xml
CopyFile /etc/libvirt/nwfilter/allow-dhcp-server.xml
CopyFile /etc/libvirt/nwfilter/allow-dhcp.xml
CopyFile /etc/libvirt/nwfilter/allow-dhcpv6-server.xml
CopyFile /etc/libvirt/nwfilter/allow-dhcpv6.xml
CopyFile /etc/libvirt/nwfilter/allow-incoming-ipv4.xml
CopyFile /etc/libvirt/nwfilter/allow-incoming-ipv6.xml
CopyFile /etc/libvirt/nwfilter/allow-ipv4.xml
CopyFile /etc/libvirt/nwfilter/allow-ipv6.xml
CopyFile /etc/libvirt/nwfilter/clean-traffic-gateway.xml
CopyFile /etc/libvirt/nwfilter/clean-traffic.xml
CopyFile /etc/libvirt/nwfilter/no-arp-ip-spoofing.xml
CopyFile /etc/libvirt/nwfilter/no-arp-mac-spoofing.xml
CopyFile /etc/libvirt/nwfilter/no-arp-spoofing.xml
CopyFile /etc/libvirt/nwfilter/no-ip-multicast.xml
CopyFile /etc/libvirt/nwfilter/no-ip-spoofing.xml
CopyFile /etc/libvirt/nwfilter/no-ipv6-multicast.xml
CopyFile /etc/libvirt/nwfilter/no-ipv6-spoofing.xml
CopyFile /etc/libvirt/nwfilter/no-mac-broadcast.xml
CopyFile /etc/libvirt/nwfilter/no-mac-spoofing.xml
CopyFile /etc/libvirt/nwfilter/no-other-l2-traffic.xml
CopyFile /etc/libvirt/nwfilter/no-other-rarp-traffic.xml
CopyFile /etc/libvirt/nwfilter/qemu-announce-self-rarp.xml
CopyFile /etc/libvirt/nwfilter/qemu-announce-self.xml
CopyFile /etc/libvirt/qemu.conf
CopyFile /etc/libvirt/qemu/networks/default.xml
CopyFile /etc/libvirt/storage/default.xml 600
CopyFile /etc/libvirt/storage/libvirt-images.xml 600
CopyFile /etc/libvirt/storage/pool.xml 600
CopyFile /etc/libvirt/virtinterfaced.conf
CopyFile /etc/libvirt/virtlxcd.conf
CopyFile /etc/libvirt/virtnetworkd.conf
CopyFile /etc/libvirt/virtnodedevd.conf
CopyFile /etc/libvirt/virtnwfilterd.conf
CopyFile /etc/libvirt/virtproxyd.conf
CopyFile /etc/libvirt/virtqemud.conf
CopyFile /etc/libvirt/virtsecretd.conf
CopyFile /etc/libvirt/virtstoraged.conf
CopyFile /etc/libvirt/virtvboxd.conf
CopyFile /etc/modprobe.d/vfio.conf
CreateDir /etc/libvirt/secrets 700
CreateLink /etc/libvirt/qemu/networks/autostart/default.xml /etc/libvirt/qemu/networks/default.xml
CreateLink /etc/libvirt/storage/autostart/default.xml /etc/libvirt/storage/default.xml
CreateLink /etc/libvirt/storage/autostart/libvirt-images.xml /etc/libvirt/storage/libvirt-images.xml
CreateLink /etc/libvirt/storage/autostart/pool.xml /etc/libvirt/storage/pool.xml


# Services
CreateLink /etc/systemd/system/multi-user.target.wants/libvirtd.service /usr/lib/systemd/system/libvirtd.service
CreateLink /etc/systemd/system/sockets.target.wants/libvirtd-ro.socket /usr/lib/systemd/system/libvirtd-ro.socket
CreateLink /etc/systemd/system/sockets.target.wants/libvirtd.socket /usr/lib/systemd/system/libvirtd.socket
CreateLink /etc/systemd/system/sockets.target.wants/virtlockd.socket /usr/lib/systemd/system/virtlockd.socket
CreateLink /etc/systemd/system/sockets.target.wants/virtlogd.socket /usr/lib/systemd/system/virtlogd.socket


# vms
CopyFile /etc/libvirt/qemu/win10.xml 600

