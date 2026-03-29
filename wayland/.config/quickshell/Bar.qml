import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Hyprland
import Quickshell.Wayland
import qs.service
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

        // LEFT: workspaces + window title
        RowLayout {
            anchors {
                left: parent.left
                top: parent.top
                bottom: parent.bottom
            }
            spacing: 0

            WorkspaceBar {
                monitor: root.hyprlandMonitor
            }

            Separator {}

            WindowTitle { monitor: root.hyprlandMonitor }
        }


        // RIGHT: everything else
        RowLayout {
            anchors {
                right: parent.right
                top: parent.top
                bottom: parent.bottom
            }
            spacing: 0

            MprisWidget { id: mprisWidget }

            Separator { visible: mprisWidget.visible }

            KvmStatus { id: kvmWidget }
            // Separator { visible: kvmWidget.visible }


            NetWidget { direction: "UPL"; iface: Config.netIface }
            Separator {}
            NetWidget { direction: "DNL"; iface: Config.netIface }

            Separator {}

            AudioWidget { label: "VOL"; isMic: false }
            Separator {}
            AudioWidget { label: "MIC"; isMic: true }

            Separator {}

            CpuWidget { barWindow: root }
            Separator {}

            MemWidget { barWindow: root }
            Separator {}

            StatWidget {
                label: "TMP"
                pollCmd: ["awk", "{printf \"%2d\", int($1/1000)}", Config.tempInput]
                suffix: "°C"
                interval: 2000
            }

            Separator {}

            TrayWidget { barWindow: root }

            Separator {}

            ClockWidget {}
        }
    }
}
