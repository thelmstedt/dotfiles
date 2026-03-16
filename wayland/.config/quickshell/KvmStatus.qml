// KvmStatus.qml
import QtQuick
import QtQuick.Layouts
import Quickshell.Io
import qs.service
import qs.component

RowLayout {
    id: root
    spacing: 0

    property bool _local: true

    Rectangle {
        implicitWidth: kvmLbl.implicitWidth + 8
        implicitHeight: parent.height
        color: Theme.alert
        visible: !root._local

        StyledText {
            id: kvmLbl
            anchors.centerIn: parent
            text: "LAP"
            color: Theme.label_fg
            font.weight: Font.Bold
        }
    }

    Process {
        id: proc
        command: ["bash", "-c",
            "grep -qsE '04d9|046d' /sys/bus/usb/devices/*/idVendor && echo local || echo remote"]
        stdout: StdioCollector {
            onStreamFinished: root._local = (this.text.trim() === "local")
        }
    }

    Timer {
        interval: 1000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: if (!proc.running) proc.running = true
    }
}
