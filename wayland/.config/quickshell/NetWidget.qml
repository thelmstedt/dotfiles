// NetWidget.qml
import QtQuick
import QtQuick.Layouts
import Quickshell.Io
import qs.service
import qs.component

Item {
    id: root
    required property string direction  // "UPL" or "DNL"
    property string iface: "enp6s0"

    implicitWidth: row.implicitWidth
    implicitHeight: Theme.barHeight

    property real _prev: -1
    property string _value: "000B/s"

    Rectangle {
        anchors.fill: parent
        color: ma.containsMouse ? Theme.dim : "transparent"
    }

    RowLayout {
        id: row
        anchors.verticalCenter: parent.verticalCenter
        spacing: 0

        TlaLabel { text: root.direction }

        StyledText {
            leftPadding: 6
            rightPadding: 10
            text: root._value
            font.weight: Font.Bold
            verticalAlignment: Text.AlignVCenter
        }
    }

    MouseArea {
        id: ma
        anchors.fill: parent
        hoverEnabled: true
    }

    Process {
        id: proc
        command: ["cat", "/sys/class/net/" + root.iface + "/statistics/" +
                         (root.direction === "UPL" ? "tx_bytes" : "rx_bytes")]
        stdout: StdioCollector {
            onStreamFinished: {
                const now = parseInt(this.text.trim())
                if (root._prev >= 0) {
                    const bps = now - root._prev
                    if (bps >= 1048576)
                        root._value = String(Math.floor(bps / 1048576)).padStart(3, "0") + "M/s"
                    else if (bps >= 1024)
                        root._value = String(Math.floor(bps / 1024)).padStart(3, "0") + "K/s"
                    else
                        root._value = String(Math.max(0, bps)).padStart(3, "0") + "B/s"
                }
                root._prev = now
            }
        }
    }

    Timer {
        interval: 1000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: proc.running = true
    }
}
