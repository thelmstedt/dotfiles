
# USB DAC line noise on boot

Noise comes through till 

     systemd[1]: Reached target Sound Card.

What we'll do is disable the module, then let systemd slot it in

blacklist:

    # /etc/modprobe.d/dac.conf
    blacklist snd_usb_audio

systemd:

    # /etc/systemd/system/dac-late.service
    [Unit]
    Description=Load USB DAC after sound target
    After=sound.target
    
    [Service]
    Type=oneshot
    ExecStart=/usr/bin/modprobe snd_usb_audio
    
    [Install]
    WantedBy=sound.target