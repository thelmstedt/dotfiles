// IoWidget.qml — disk I/O rates from /proc/diskstats
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

    property var _prevRead:    ({})
    property var _prevWrite:   ({})
    property var _prevIoTicks: ({})
    property var _mountPoints: ({})  // disk -> [mount, ...]
    property string _rVal: "000B"
    property string _wVal: "000B"
    property var _diskRates: []      // [{name, read, write, util, queue, mounts}, ...]
    property var _readHistory: []
    property var _writeHistory: []

    function _fmt(bps) {
        if (bps >= 1000000000) return String(Math.floor(bps / 1000000000)).padStart(3, "0") + "G"
        if (bps >= 1000000)    return String(Math.floor(bps / 1000000)).padStart(3, "0") + "M"
        if (bps >= 1000)       return String(Math.floor(bps / 1000)).padStart(3, "0") + "K"
        return String(Math.max(0, bps)).padStart(3, "0") + "B"
    }

    Rectangle {
        anchors.fill: parent
        color: ma.containsMouse ? Theme.dim : "transparent"
    }

    RowLayout {
        id: row
        anchors.verticalCenter: parent.verticalCenter
        spacing: 0

        TlaLabel { text: "IO" }

        StyledText {
            leftPadding: 6
            rightPadding: 2
            text: root._rVal + "r"
            font.weight: Font.Bold
            verticalAlignment: Text.AlignVCenter
        }

        StyledText {
            rightPadding: 4
            text: root._wVal + "w"
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
        implicitWidth: grid.implicitWidth + 16
        implicitHeight: ioPopupCol.implicitHeight + grid.implicitHeight + 12

        Column {
            id: ioPopupCol
            anchors { top: parent.top; left: parent.left; topMargin: 6; leftMargin: 8 }
            spacing: 3

            SparkLine {
                width: ioPopupCol.implicitWidth
                height: 30
                values: root._readHistory
                lineColor: Theme.label_bg
            }

            SparkLine {
                width: ioPopupCol.implicitWidth
                height: 30
                values: root._writeHistory
                lineColor: Theme.alert
            }

            Item { width: 1; height: 3 }
        }

        GridLayout {
            id: grid
            anchors { top: ioPopupCol.bottom; left: parent.left; topMargin: 0; leftMargin: 8 }
            columns: 6
            columnSpacing: 10
            rowSpacing: 3

            Repeater {
                model: root._diskRates

                delegate: Item {
                    required property var modelData
                    required property int index
                    Layout.columnSpan: 6
                    implicitHeight: diskRow.implicitHeight
                    implicitWidth: diskRow.implicitWidth

                    Row {
                        id: diskRow
                        spacing: 10

                        // Device name
                        StyledText {
                            text: modelData.name
                            width: 80
                            color: Theme.label_bg
                            font.weight: Font.Bold
                            verticalAlignment: Text.AlignVCenter
                        }

                        // Mount points
                        StyledText {
                            text: modelData.mounts.length > 0 ? modelData.mounts.join("  ") : "—"
                            width: 140
                            color: Theme.fg
                            elide: Text.ElideRight
                            verticalAlignment: Text.AlignVCenter
                        }

                        // Utilization %
                        StyledText {
                            text: String(modelData.util).padStart(3, "0") + "%"
                            width: 40
                            horizontalAlignment: Text.AlignRight
                            color: modelData.util > 80 ? Theme.alert : Theme.fg
                            font.weight: modelData.util > 80 ? Font.Bold : Font.Normal
                            verticalAlignment: Text.AlignVCenter
                        }

                        // Queue depth
                        StyledText {
                            text: "q:" + modelData.queue
                            width: 32
                            color: modelData.queue > 4 ? Theme.alert : Theme.dim
                            verticalAlignment: Text.AlignVCenter
                        }

                        // Read
                        StyledText {
                            text: root._fmt(modelData.read) + "r"
                            width: 50
                            horizontalAlignment: Text.AlignRight
                            color: Theme.fg
                            verticalAlignment: Text.AlignVCenter
                        }

                        // Write
                        StyledText {
                            text: root._fmt(modelData.write) + "w"
                            width: 50
                            horizontalAlignment: Text.AlignRight
                            color: Theme.fg
                            verticalAlignment: Text.AlignVCenter
                        }
                    }
                }
            }
        }
    }

    FileView {
        id: mountsFile
        path: "/proc/mounts"
        onTextChanged: {
            const mp = {}
            text().split('\n').forEach(line => {
                const p = line.trim().split(/\s+/)
                if (p.length < 2 || !p[0].startsWith('/dev/')) return
                const dev = p[0].substring(5)  // strip /dev/
                // Map partition to whole disk: sda1->sda, nvme0n1p1->nvme0n1
                const disk = dev.replace(/p\d+$/, '').replace(/\d+$/, '')
                if (!mp[disk]) mp[disk] = []
                mp[disk].push(p[1])
            })
            root._mountPoints = mp
        }
    }

    FileView {
        id: diskstats
        path: "/proc/diskstats"
        onTextChanged: {
            let totalRead = 0, totalWrite = 0
            const newR = {}, newW = {}, newT = {}
            const rates = []

            text().split('\n').forEach(line => {
                const p = line.trim().split(/\s+/)
                if (p.length < 14) return
                const name = p[2]
                if (!/^(sd[a-z]|hd[a-z]|nvme\d+n\d+|vd[a-z]|xvd[a-z])$/.test(name)) return

                const secR    = parseInt(p[5])
                const secW    = parseInt(p[9])
                const queue   = parseInt(p[11])
                const ioTicks = parseInt(p[12])

                newR[name] = secR
                newW[name] = secW
                newT[name] = ioTicks

                let read = 0, write = 0, util = 0
                if (name in root._prevRead) {
                    read  = Math.max(0, (secR    - root._prevRead[name])    * 512)
                    write = Math.max(0, (secW    - root._prevWrite[name])   * 512)
                    util  = Math.min(100, Math.max(0,
                                Math.round((ioTicks - root._prevIoTicks[name]) / 10)))
                    totalRead  += read
                    totalWrite += write
                }

                rates.push({
                    name,
                    read, write, util, queue,
                    mounts: root._mountPoints[name] ?? []
                })
            })

            root._rVal         = root._fmt(totalRead)
            root._wVal         = root._fmt(totalWrite)
            root._prevRead     = newR
            root._prevWrite    = newW
            root._prevIoTicks  = newT
            root._diskRates    = rates
            root._readHistory  = root._readHistory.concat([totalRead]).slice(-60)
            root._writeHistory = root._writeHistory.concat([totalWrite]).slice(-60)
        }
    }

    Timer {
        interval: 1000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: { diskstats.reload(); mountsFile.reload() }
    }
}
