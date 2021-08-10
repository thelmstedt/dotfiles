
AddPackage feh # Fast and light imlib2-based image viewer
AddPackage xcompmgr # Composite Window-effects manager for X.org
AddPackage --foreign transset-df # A patched version of X.Org's transset with added functionality.

RemovePackage ebtables
AddPackage --foreign ebtables # Ethernet bridge filtering utilities

# Wed Aug 11 09:17:59 AM AEST 2021 - Extra files

RemoveFile /etc/ansible/ansible.cfg
RemoveFile /etc/ansible

# Wed Aug 11 09:17:59 AM AEST 2021 - New / changed files
CopyFile /etc/libvirt/qemu/networks/tmv0.xml 600
CopyFile /etc/libvirt/qemu/networks/tmv1.xml 600
CopyFile /etc/libvirt/qemu/networks/vagrant-libvirt.xml 600
CopyFile /etc/libvirt/qemu/tmv_test_vm.xml 600
CreateDir /etc/systemd/system/libvirtd.service.d
CreateLink /etc/systemd/system/multi-user.target.wants/pgbouncer.service /usr/lib/systemd/system/pgbouncer.service
