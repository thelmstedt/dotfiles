// StatWidget.qml — generic polled stat with TLA badge
import QtQuick
import QtQuick.Layouts
import Quickshell.Io
import qs.service
import qs.component

Item {
    id: root
    required property string label
    required property list<string> pollCmd
    required property string suffix
    property int interval: 1000

    implicitWidth: row.implicitWidth
    implicitHeight: Theme.barHeight

    property string _value: "--"

    Rectangle {
        anchors.fill: parent
        color: ma.containsMouse ? Theme.dim : "transparent"
    }

    RowLayout {
        id: row
        anchors.verticalCenter: parent.verticalCenter
        spacing: 0

        TlaLabel { text: root.label }

        StyledText {
            leftPadding: 6
            rightPadding: 4
            text: root._value + root.suffix
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
        command: root.pollCmd
        stdout: StdioCollector {
            onStreamFinished: root._value = this.text.trim()
        }
    }

    Timer {
        interval: root.interval
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: if (!proc.running) proc.running = true
    }
}
