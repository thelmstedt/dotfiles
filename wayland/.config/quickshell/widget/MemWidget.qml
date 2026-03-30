// MemWidget.qml — memory usage with hover popup breakdown
import QtQuick
import QtQuick.Layouts
import Quickshell.Io
import qs.service
import qs.component

Item {
    id: root
    required property var barWindow
    implicitWidth: row.implicitWidth
    implicitHeight: Theme.barHeight

    property string _pct: " --"
    property var _history: []
    property var _stats: ({ total: 0, used: 0, available: 0, buffers: 0, cached: 0, swapTotal: 0, swapUsed: 0 })

    function _gb(kb) { return (kb / 1048576).toFixed(1) }

    Rectangle {
        anchors.fill: parent
        color: ma.containsMouse ? Theme.dim : "transparent"
    }

    RowLayout {
        id: row
        anchors.verticalCenter: parent.verticalCenter
        spacing: 0

        TlaLabel { text: "MEM" }

        StyledText {
            leftPadding: 6
            rightPadding: 4
            text: root._pct + "%"
            font.weight: Font.Bold
            verticalAlignment: Text.AlignVCenter
        }
    }

    MouseArea {
        id: ma
        anchors.fill: parent
        hoverEnabled: true
        onEntered: popup.show(root)
        onExited: popup.hide()
    }

    BarPopup {
        id: popup
        barWindow: root.barWindow
        implicitWidth: col.implicitWidth + 16
        implicitHeight: col.implicitHeight + 12

        property var s: root._stats

        Column {
            id: col
            anchors { top: parent.top; left: parent.left; topMargin: 6; leftMargin: 8 }
            spacing: 6

            SparkLine {
                width: col.implicitWidth
                height: 40
                values: root._history
                maxVal: 100
                lineColor: Theme.label_bg
            }

            GridLayout {
                columns: 3
                columnSpacing: 10
                rowSpacing: 3

                StyledText { text: "Used";      color: Theme.label_bg; font.weight: Font.Bold }
                Rectangle {
                    width: 100; height: 12
                    Layout.alignment: Qt.AlignVCenter
                    color: Theme.dim
                    Rectangle {
                        width: popup.s.total > 0 ? parent.width * popup.s.used / popup.s.total : 0
                        height: parent.height
                        color: (popup.s.used / popup.s.total) > 0.8 ? Theme.alert : Theme.label_bg
                    }
                }
                StyledText { text: root._gb(popup.s.used) + " / " + root._gb(popup.s.total) + " GB"; color: Theme.fg }

                StyledText { text: "Avail";     color: Theme.fg }
                Item { width: 100; height: 12 }
                StyledText { text: root._gb(popup.s.available) + " GB"; color: Theme.fg }

                StyledText { text: "Buf/Cache"; color: Theme.fg }
                Item { width: 100; height: 12 }
                StyledText { text: root._gb(popup.s.buffers + popup.s.cached) + " GB"; color: Theme.fg }

                StyledText { text: "Swap";      color: Theme.fg }
                Rectangle {
                    width: 100; height: 12
                    Layout.alignment: Qt.AlignVCenter
                    color: Theme.dim
                    Rectangle {
                        width: popup.s.swapTotal > 0 ? parent.width * popup.s.swapUsed / popup.s.swapTotal : 0
                        height: parent.height
                        color: Theme.label_bg
                    }
                }
                StyledText { text: root._gb(popup.s.swapUsed) + " / " + root._gb(popup.s.swapTotal) + " GB"; color: Theme.fg }
            }
        }
    }

    FileView {
        id: memFile
        path: "/proc/meminfo"
        onTextChanged: {
            const vals = {}
            text().split('\n').forEach(line => {
                const m = line.match(/^(\w+):\s+(\d+)/)
                if (m) vals[m[1]] = parseInt(m[2])
            })
            const total     = vals["MemTotal"]     ?? 0
            const available = vals["MemAvailable"] ?? 0
            const buffers   = vals["Buffers"]      ?? 0
            const cached    = (vals["Cached"] ?? 0) + (vals["SReclaimable"] ?? 0)
            const swapTotal = vals["SwapTotal"]    ?? 0
            const swapFree  = vals["SwapFree"]     ?? 0
            const used = total - available

            root._stats = { total, used, available, buffers, cached, swapTotal, swapUsed: swapTotal - swapFree }
            const pct = total > 0 ? Math.round(used / total * 100) : 0
            root._pct = String(pct).padStart(3)
            root._history = root._history.concat([pct]).slice(-60)
        }
    }

    Timer {
        interval: 2000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: memFile.reload()
    }
}
