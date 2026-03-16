// CpuWidget.qml — native /proc/stat diff, no subshell
import QtQuick
import QtQuick.Layouts
import Quickshell.Io
import qs.service
import qs.component

Item {
    id: root
    implicitWidth: row.implicitWidth
    implicitHeight: Theme.barHeight

    property real _prevIdle: -1
    property real _prevTotal: -1
    property string _value: "  0"

    Rectangle {
        anchors.fill: parent
        color: ma.containsMouse ? Theme.dim : "transparent"
    }

    RowLayout {
        id: row
        anchors.verticalCenter: parent.verticalCenter
        spacing: 0

        TlaLabel { text: "CPU" }

        StyledText {
            leftPadding: 6
            rightPadding: 10
            text: root._value + "%"
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
        command: ["cat", "/proc/stat"]
        stdout: StdioCollector {
            onStreamFinished: {
                // first line: "cpu  user nice system idle iowait irq softirq ..."
                const parts = this.text.split('\n')[0].trim().split(/\s+/).slice(1).map(Number)
                const idle  = parts[3] + parts[4]  // idle + iowait
                const total = parts.reduce((a, b) => a + b, 0)
                if (root._prevTotal >= 0) {
                    const dt = total - root._prevTotal
                    const di = idle  - root._prevIdle
                    const pct = dt > 0 ? Math.round((1 - di / dt) * 100) : 0
                    root._value = String(Math.max(0, Math.min(100, pct))).padStart(3)
                }
                root._prevTotal = total
                root._prevIdle  = idle
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
