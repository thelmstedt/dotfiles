# Setup

Manage AUR with `yay`

    git clone https://aur.archlinux.org/yay-git.git
    makepkg -si

Manage machine setup with `aconfmgr` and user setup with `stow`

    yay -S stow aconfmgr

`stow` what you want into your $HOME

    stow zsh bin machine_base x11 prime

Provided we've stowed `~/.config/aconfmgr` in the previous step - we can restore our machine config

    aconfmgr apply



# App Notes

## fonts

https://old.reddit.com/r/archlinux/comments/5r5ep8/make_your_arch_fonts_beautiful_easily/

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

# VFIO Notes

For Prime - AMD 3950x

## Passthrough

Details of card to be passed-through

    $ lspci -nn | grep Radeon
    
    09:00.0 VGA compatible controller [0300]: Advanced Micro Devices, Inc. [AMD/ATI] Ellesmere [Radeon RX 470/480/570/570X/580/580X/590] [1002:67df] (rev ef)
    09:00.1 Audio device [0403]: Advanced Micro Devices, Inc. [AMD/ATI] Ellesmere HDMI Audio [Radeon RX 470/480 / 570/580/590] [1002:aaf0]


Create `/etc/modprobe.d/vfio.conf` with

    options vfio-pci ids=1002:67df,1002:aaf0


## Evdev

Keyboard: `/dev/input/by-id/usb-04d9_USB_Keyboard-event-kbd`

Mouse: `/dev/input/by-id/usb-Logitech_USB_Receiver-if02-event-mouse`


    <qemu:commandline>
        <qemu:arg value='-object'/>
        <qemu:arg value='input-linux,id=mouse1,evdev=/dev/input/by-id/usb-Logitech_USB_Receiver-if02-event-mouse'/>
        <qemu:arg value='-object'/>
        <qemu:arg value='input-linux,id=kbd1,evdev=/dev/input/by-id/usb-04d9_USB_Keyboard-event-kbd,grab_all=on,repeat=on'/>
    </qemu:commandline>


## Pulseaudio

    <qemu:commandline>
      <qemu:env name='QEMU_AUDIO_DRV' value='pa'/>
      <qemu:env name='QEMU_PA_SAMPLES' value='8192'/>
      <qemu:env name='QEMU_AUDIO_TIMER_PERIOD' value='99'/>
      <qemu:env name='QEMU_PA_SERVER' value='/run/user/1000/pulse/native'/>
    </qemu:commandline>


## Cpu Pinning

`lscpu` tells us our cores/threads with grouping

    $ lscpu -e
    
    CPU NODE SOCKET CORE L1d:L1i:L2:L3 ONLINE    MAXMHZ    MINMHZ
      0    0      0    0 0:0:0:0          yes 3500.0000 2200.0000
      1    0      0    1 1:1:1:0          yes 3500.0000 2200.0000
      2    0      0    2 2:2:2:0          yes 3500.0000 2200.0000
      3    0      0    3 3:3:3:0          yes 3500.0000 2200.0000
      4    0      0    4 4:4:4:1          yes 3500.0000 2200.0000
      5    0      0    5 5:5:5:1          yes 3500.0000 2200.0000
      6    0      0    6 6:6:6:1          yes 3500.0000 2200.0000
      7    0      0    7 7:7:7:1          yes 3500.0000 2200.0000
      8    0      0    8 8:8:8:2          yes 3500.0000 2200.0000
      9    0      0    9 9:9:9:2          yes 3500.0000 2200.0000
     10    0      0   10 10:10:10:2       yes 3500.0000 2200.0000
     11    0      0   11 11:11:11:2       yes 3500.0000 2200.0000
     12    0      0   12 12:12:12:3       yes 3500.0000 2200.0000
     13    0      0   13 13:13:13:3       yes 3500.0000 2200.0000
     14    0      0   14 14:14:14:3       yes 3500.0000 2200.0000
     15    0      0   15 15:15:15:3       yes 3500.0000 2200.0000
     16    0      0    0 0:0:0:0          yes 3500.0000 2200.0000
     17    0      0    1 1:1:1:0          yes 3500.0000 2200.0000
     18    0      0    2 2:2:2:0          yes 3500.0000 2200.0000
     19    0      0    3 3:3:3:0          yes 3500.0000 2200.0000
     20    0      0    4 4:4:4:1          yes 3500.0000 2200.0000
     21    0      0    5 5:5:5:1          yes 3500.0000 2200.0000
     22    0      0    6 6:6:6:1          yes 3500.0000 2200.0000
     23    0      0    7 7:7:7:1          yes 3500.0000 2200.0000
     24    0      0    8 8:8:8:2          yes 3500.0000 2200.0000
     25    0      0    9 9:9:9:2          yes 3500.0000 2200.0000
     26    0      0   10 10:10:10:2       yes 3500.0000 2200.0000
     27    0      0   11 11:11:11:2       yes 3500.0000 2200.0000
     28    0      0   12 12:12:12:3       yes 3500.0000 2200.0000
     29    0      0   13 13:13:13:3       yes 3500.0000 2200.0000
     30    0      0   14 14:14:14:3       yes 3500.0000 2200.0000
     31    0      0   15 15:15:15:3       yes 3500.0000 2200.0000


Take the cpus in pairs with matching `CORE`. Here we want 8 cvpu: 4 cores / 2 threads a core.

    <vcpu placement='static'>8</vcpu>
    <iothreads>1</iothreads>
    <cputune>
        <vcpupin vcpu='0' cpuset='2'/>
        <vcpupin vcpu='1' cpuset='18'/>
        <vcpupin vcpu='2' cpuset='3'/>
        <vcpupin vcpu='3' cpuset='19'/>
        <vcpupin vcpu='4' cpuset='4'/>
        <vcpupin vcpu='5' cpuset='20'/>
        <vcpupin vcpu='6' cpuset='5'/>
        <vcpupin vcpu='7' cpuset='21'/>
        <emulatorpin cpuset='0,16'/>
        <iothreadpin iothread='1' cpuset='0,6'/>
    </cputune>

AMD hyperthreading fix

    <cpu mode='host-passthrough' check='none'>
     <topology sockets='1' cores='4' threads='2'/>
     <feature policy='require' name='topoext'/>
    </cpu>
    