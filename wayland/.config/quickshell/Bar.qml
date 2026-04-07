import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import Quickshell.Wayland
import qs.service
import qs.component
import qs.widget

PanelWindow {
    WlrLayershell.screen: root.screen
    id: root
    required property var screen

    anchors {
        top: true
        left: true
        right: true
    }
    implicitHeight: 30
    color: Theme.bg

    // Track which monitor this bar is on for workspace filtering
    property var hyprlandMonitor: Hyprland.monitorFor(root.screen)

    Item {
        anchors.fill: parent

        // LEFT: workspaces + mpris + window title (bounded by right group)
        RowLayout {
            anchors {
                left: parent.left
                top: parent.top
                bottom: parent.bottom
                right: rightRow.left
            }
            spacing: 0

            WorkspaceBar {
                monitor: root.hyprlandMonitor
            }

            Separator {}

            WindowTitle {
                monitor: root.hyprlandMonitor
                Layout.fillWidth: true
            }
        }


        // RIGHT: everything else
        RowLayout {
            id: rightRow
            anchors {
                right: parent.right
                top: parent.top
                bottom: parent.bottom
            }
            spacing: 0

            MprisWidget { id: mprisWidget }

            Separator { visible: mprisWidget.visible }

            SinkWidget {}

            Separator {}

            NetWidget { barWindow: root }

            Separator {}

            SysWidget { barWindow: root }

            Separator {}

            TrayWidget { barWindow: root }

              Separator {}

            ClockWidget {}
        }
    }
}
