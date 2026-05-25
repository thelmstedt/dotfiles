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
    implicitHeight: Theme.barHeight
    color: Theme.bg

    // Track which monitor this bar is on for workspace filtering
    property var hyprlandMonitor: Hyprland.monitorFor(root.screen)

    Item {
        id: topRow
        anchors {
            top: parent.top
            left: parent.left
            right: parent.right
        }
        height: Theme.barHeight

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

            Separator {}

            Item {
                Layout.fillWidth: true
                Layout.fillHeight: true
                clip: true

                WindowTitle {
                    monitor: root.hyprlandMonitor
                    visible: !tabbedTabs.active
                    anchors.fill: parent
                }

                TabbedTabs {
                    id: tabbedTabs
                    monitor: root.hyprlandMonitor
                    anchors {
                        top: parent.top
                        bottom: parent.bottom
                        left: parent.left
                        right: parent.right
                    }
                }
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
