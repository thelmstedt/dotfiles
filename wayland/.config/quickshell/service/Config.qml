pragma Singleton
import QtQuick
import Quickshell

Singleton {
    readonly property string netIface:   "enp6s0"
    readonly property string tempInput:  "/sys/class/hwmon/hwmon3/temp1_input"
}
