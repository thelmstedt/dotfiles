// WorkspaceBar.qml
import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import qs.service
import qs.component

RowLayout {
    id: root
    required property var monitor
    spacing: 0

    Repeater {
        model: Hyprland.workspaces

        delegate: Rectangle {
            required property var modelData
            property bool isActive: modelData.id === (root.monitor?.activeWorkspace?.id ?? -1)

            implicitWidth: wsLabel.implicitWidth + 16
            implicitHeight: Theme.barHeight
            color: isActive ? Theme.fg : (hover.containsMouse ? Theme.dim : Theme.bg)

            StyledText {
                id: wsLabel
                anchors.centerIn: parent
                text: modelData.id
                color: isActive ? Theme.bg : (hover.containsMouse ? Theme.fg : Theme.dim)
                font.weight: Font.Bold
            }

            MouseArea {
                id: hover
                anchors.fill: parent
                hoverEnabled: true
                onClicked: Hyprland.dispatch("workspace " + modelData.id)
            }
        }
    }
}
