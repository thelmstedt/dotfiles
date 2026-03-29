# logitech mouse config

The ratchet/smooth change is called "smartshift" and it sucks by default, at least for this mouse

use `logid` from `logiops`

    $ yay -S logiops

see what it finds:

    $ sudo logid -v
    [INFO] Config file does not exist, using empty config.
    [DEBUG] Unsupported device /dev/hidraw1 ignored
    [DEBUG] Unsupported device /dev/hidraw2 ignored
    [DEBUG] Unsupported device /dev/hidraw3 ignored
    [DEBUG] Unsupported device /dev/hidraw4 ignored
    [INFO] Detected receiver at /dev/hidraw5
    [DEBUG] Unsupported device /dev/hidraw0 ignored
    [INFO] Device found: MX Master 3S on /dev/hidraw6:255
    [DEBUG] /dev/hidraw6:255 remappable buttons:
    [DEBUG] CID  | reprog? | fn key? | mouse key? | gesture support?
    [DEBUG] 0x50 |         |         | YES        |
    [DEBUG] 0x51 |         |         | YES        |
    [DEBUG] 0x52 | YES     |         | YES        | YES
    [DEBUG] 0x53 | YES     |         | YES        | YES
    [DEBUG] 0x56 | YES     |         | YES        | YES
    [DEBUG] 0xc3 | YES     |         | YES        | YES
    [DEBUG] 0xc4 | YES     |         | YES        | YES
    [DEBUG] 0xd7 | YES     |         |            | YES
    [DEBUG] Thumb wheel detected (0x2150), capabilities:
    [DEBUG] timestamp | touch | proximity | single tap
    [DEBUG] YES       | YES   | YES       | YES
    [DEBUG] Thumb wheel resolution: native (18), diverted (120)

Now you can configure smartshift

    $ cat /etc/logid.cfg
    devices: ({
    name: "MX Master 3S";
    smartshift: { on: true; threshold: 20; };
    hiresscroll: { hires: true; invert: false; target: false; };
    dpi: 1000;
    buttons: ();
    });

and enable the service

    sudo systemctl enable --now logid