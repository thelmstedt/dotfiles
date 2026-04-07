// SinkWidget.qml — active sink label + volume, click to switch, scroll to adjust
import QtQuick
import QtQuick.Layouts
import Quickshell.Io
import Quickshell.Services.Pipewire
import qs.service
import qs.component

Item {
    id: root
    implicitWidth: row.implicitWidth
    implicitHeight: Theme.barHeight

    readonly property string _headphones: "alsa_output.usb-JDS_Labs_JDS_Labs_Atom_DAC_2-00.iec958-stereo"
    readonly property string _speakers:   "alsa_output.usb-SMSL_AUDIO_SMSL_AD18_AMP-00.iec958-stereo"

    property string _current: _sink?.name ?? ""
    property string _label: {
        if (_current === _headphones) return "HPH"
        if (_current === _speakers)   return "SPK"
        return "???"
    }

    property var  _sink:  Pipewire.defaultAudioSink
    property bool _muted: _sink?.audio.muted ?? false
    property int  _vol:   _sink ? Math.round(_sink.audio.volume * 100) : 0

    PwObjectTracker { objects: [Pipewire.defaultAudioSink] }

    Rectangle {
        anchors.fill: parent
        color: ma.containsMouse ? Theme.dim : "transparent"
    }

    RowLayout {
        id: row
        anchors.verticalCenter: parent.verticalCenter
        spacing: 0

        Rectangle {
            implicitWidth: lbl.implicitWidth + 8
            implicitHeight: parent.height
            color: root._muted ? Theme.alert : Theme.label_bg
            StyledText {
                id: lbl
                anchors.centerIn: parent
                text: root._label
                color: Theme.label_fg
                font.weight: Font.Bold
            }
        }

        StyledText {
            leftPadding: 6
            rightPadding: 4
            text: root._vol + "%"
            font.weight: Font.Bold
            verticalAlignment: Text.AlignVCenter
        }
    }

    MouseArea {
        id: ma
        anchors.fill: parent
        hoverEnabled: true
        acceptedButtons: Qt.LeftButton | Qt.RightButton
        scrollGestureEnabled: false
        onClicked: (event) => {
            if (event.button === Qt.RightButton) {
                if (!pavucontrol.running) pavucontrol.running = true
            } else {
                toggle.running = true
            }
        }
        onWheel: (event) => {
            if (!root._sink) return
            const delta = event.angleDelta.y > 0 ? 0.05 : -0.05
            root._sink.audio.volume = Math.max(0, Math.min(1, root._sink.audio.volume + delta))
        }
    }

    Process {
        id: toggle
        command: ["switch_pulse_sinks"]
    }

    Process {
        id: pavucontrol
        command: ["pavucontrol"]
    }


}
