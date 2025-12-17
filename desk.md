# linak controller

Scan for the desk

    $ uvx linak-controller --scan
    Found 1 devices using hci0
    F7:7B:4D:D9:B1:6B: Desk 5419

Now we can control it

    $ uvx linak-controller --mac-address F7:7B:4D:D9:B1:6B --move-to 1150

## pairing

    $ bluetoothctl
    > remove F7:7B:4D:D9:B1:6B
    > scan on
    > scan off
    > pair F7:7B:4D:D9:B1:6B
    > trust F7:7B:4D:D9:B1:6B
    > exit


## debugging

Start `sudo btmon` and watch the traffic as you issue commands