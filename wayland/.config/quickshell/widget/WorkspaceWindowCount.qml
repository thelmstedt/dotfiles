import QtQuick
import QtQuick.Layouts
import Quickshell.Io
import qs.service
import qs.component

Item {
    id: root
    required property var monitor

    readonly property int count: root.monitor?.activeWorkspace?.toplevels?.values?.length ?? 0
    readonly property int workspaceId: root.monitor?.activeWorkspace?.id ?? -1
    readonly property string layoutName: root.currentLayoutName()

    function currentLayoutName() {
        const lines = layoutFile.text().split(/\n/)
        for (const line of lines) {
            const parts = line.trim().split(/\s+/)
            if (parts.length >= 2 && parts[0] === String(root.workspaceId))
                return parts[1]
        }

        return ""
    }

    implicitWidth: row.implicitWidth
    implicitHeight: Theme.barHeight

    Rectangle {
        anchors.fill: parent
        color: hover.containsMouse ? Theme.dim : "transparent"
    }

    RowLayout {
        id: row
        anchors.verticalCenter: parent.verticalCenter
        spacing: 0

        TlaLabel { text: "WIN" }

        StyledText {
            leftPadding: 6
            rightPadding: root.layoutName.length > 0 ? 4 : 6
            text: root.count
            font.weight: Font.Bold
            verticalAlignment: Text.AlignVCenter
        }

        StyledText {
            visible: root.layoutName.length > 0
            leftPadding: 0
            rightPadding: 6
            text: root.layoutName
            color: Theme.dim
            font.weight: Font.Bold
            verticalAlignment: Text.AlignVCenter
        }
    }

    MouseArea {
        id: hover
        anchors.fill: parent
        hoverEnabled: true
    }

    FileView {
        id: layoutFile
        path: "/tmp/xmonad-workspace-layouts"
    }

    Timer {
        interval: 250
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: layoutFile.reload()
    }
}
