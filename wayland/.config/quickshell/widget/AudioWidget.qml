// AudioWidget.qml — VOL or MIC via pipewire
import QtQuick
import QtQuick.Layouts
import Quickshell.Io
import Quickshell.Services.Pipewire
import qs.service
import qs.component

Item {
    id: root
    required property string label
    required property bool isMic

    implicitWidth: row.implicitWidth
    implicitHeight: Theme.barHeight

    property var _node: root.isMic ? Pipewire.defaultAudioSource : Pipewire.defaultAudioSink
    property bool _muted: _node?.audio.muted ?? false
    property int _vol: _node ? Math.round(_node.audio.volume * 100) : 0

    PwObjectTracker { objects: [Pipewire.defaultAudioSink, Pipewire.defaultAudioSource] }

    Rectangle {
        anchors.fill: parent
        color: ma.containsMouse ? Theme.dim : "transparent"
    }

    RowLayout {
        id: row
        anchors.verticalCenter: parent.verticalCenter
        spacing: 0

        Rectangle {
            implicitWidth: muteLbl.implicitWidth + 8
            implicitHeight: parent.height
            color: root._muted ? Theme.alert : Theme.label_bg
            StyledText {
                id: muteLbl
                anchors.centerIn: parent
                text: root._muted ? (root.label + " MUT") : root.label
                color: Theme.label_fg
                font.weight: Font.Bold
            }
        }

        StyledText {
            leftPadding: 6
            rightPadding: 10
            text: root._vol + "%"
            font.weight: Font.Bold
            verticalAlignment: Text.AlignVCenter
            visible: !root._muted
        }
    }

    Process {
        id: pavucontrol
        command: ["pavucontrol"]
    }

    MouseArea {
        id: ma
        anchors.fill: parent
        hoverEnabled: true
        onClicked: pavucontrol.running = true
    }
}
