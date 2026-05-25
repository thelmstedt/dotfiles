import QtQuick
import QtQuick.Layouts
import Quickshell.Hyprland
import Quickshell.Io
import qs.service
import qs.component

Item {
    id: root
    required property var monitor

    readonly property int workspaceId: root.monitor?.activeWorkspace?.id ?? -1
    readonly property var toplevels: root.monitor?.activeWorkspace?.toplevels?.values ?? []
    readonly property var orderedToplevels: root.sortedToplevels()
    readonly property int tabCount: root.orderedToplevels.length
    readonly property bool active: root.toplevels.length > 0

    function tabWidth() {
        if (root.tabCount <= 0)
            return 0

        if (root.tabCount === 1)
            return Math.min(root.width, 300)

        const available = root.width + root.tabCount - 1
        return Math.min(180, available / root.tabCount)
    }

    function normalizedAddress(address) {
        const raw = String(address ?? "").toLowerCase()
        return raw.startsWith("0x") ? raw.slice(2) : raw
    }

    function workspaceOrder() {
        const lines = orderFile.text().split(/\n/)
        for (const line of lines) {
            const parts = line.trim().split(/\s+/)
            if (parts.length >= 2 && parts[0] === String(root.workspaceId))
                return parts.slice(1).map(address => root.normalizedAddress(address))
        }

        return []
    }

    function sortedToplevels() {
        const order = root.workspaceOrder()
        if (order.length === 0)
            return root.toplevels

        const byAddress = {}
        for (const toplevel of root.toplevels)
            byAddress[root.normalizedAddress(toplevel?.address)] = toplevel

        const sorted = []
        const seen = {}
        for (const address of order) {
            const toplevel = byAddress[address]
            if (toplevel) {
                sorted.push(toplevel)
                seen[address] = true
            }
        }

        for (const toplevel of root.toplevels) {
            const address = root.normalizedAddress(toplevel?.address)
            if (!seen[address])
                sorted.push(toplevel)
        }

        return sorted
    }

    visible: active
    implicitWidth: Math.min(row.implicitWidth, 300 * root.tabCount)
    implicitHeight: Theme.barHeight
    clip: true

    Rectangle {
        anchors {
            left: parent.left
            right: parent.right
            bottom: parent.bottom
        }
        height: 1
        color: Theme.dim
    }

    RowLayout {
        id: row
        anchors {
            top: parent.top
            bottom: parent.bottom
            left: parent.left
        }
        width: Math.min(implicitWidth, root.width)
        spacing: -1

        Repeater {
            model: root.active ? root.orderedToplevels : []

            delegate: Item {
                required property int index
                required property var modelData
                readonly property var tab: modelData
                readonly property bool selected: tab?.activated
                readonly property real widthForTab: root.tabWidth()

                implicitHeight: root.height
                implicitWidth: widthForTab
                Layout.minimumWidth: widthForTab
                Layout.preferredWidth: widthForTab
                Layout.maximumWidth: widthForTab

                Rectangle {
                    id: tabRect
                    readonly property bool selected: parent.selected

                    anchors {
                        left: parent.left
                        right: parent.right
                        bottom: parent.bottom
                    }
                    height: selected ? root.height : root.height - 5
                    color: selected ? Theme.fg : (hover.containsMouse ? Theme.dim : Theme.bg)
                    border.width: 1
                    border.color: hover.containsMouse ? Theme.fg : Theme.dim
                    radius: 3
                    clip: true

                    Rectangle {
                        anchors {
                            top: parent.top
                            left: parent.left
                            right: parent.right
                        }
                        height: 2
                        color: Theme.dim
                    }

                    StyledText {
                        id: tabTitle
                        anchors {
                            left: parent.left
                            right: parent.right
                            verticalCenter: parent.verticalCenter
                        }
                        leftPadding: 10
                        rightPadding: 10
                        text: tab?.title ?? ""
                        color: tabRect.selected ? Theme.bg : Theme.fg
                        elide: Text.ElideRight
                        font.weight: tabRect.selected ? Font.Bold : Font.Normal
                        verticalAlignment: Text.AlignVCenter
                    }

                    MouseArea {
                        id: hover
                        anchors.fill: parent
                        hoverEnabled: true
                        onClicked: {
                            if (tab?.address) {
                                const rawAddress = String(tab.address)
                                const address = rawAddress.startsWith("0x") ? rawAddress : "0x" + rawAddress
                                Hyprland.dispatch('hl.dsp.focus({ window = "address:' + address + '" })')
                            }
                        }
                    }
                }
            }
        }
    }

    FileView {
        id: orderFile
        path: "/tmp/xmonad-workspace-order"
    }

    Timer {
        interval: 250
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: orderFile.reload()
    }
}
