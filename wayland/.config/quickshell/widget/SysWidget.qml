// SysWidget.qml — combined CPU / MEM / IO / TMP widget
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

    // ── CPU state ──────────────────────────────────────────────────────────
    property real _cpuPrevIdle:  -1
    property real _cpuPrevTotal: -1
    property int  _cpuPct:       0
    property var  _cpuHistory:   []
    property var  _prevCoreIdle:  []
    property var  _prevCoreTotal: []
    property var  _coreValues:    []   // [{label, pct}, ...]

    // ── MEM state ──────────────────────────────────────────────────────────
    property int _memPct:     0
    property var _memHistory: []
    property var _memStats:   ({ total: 0, used: 0, available: 0, buffers: 0, cached: 0, swapTotal: 0, swapUsed: 0 })

    // ── IO state ───────────────────────────────────────────────────────────
    property var    _ioPrevRead:    ({})
    property var    _ioPrevWrite:   ({})
    property var    _ioPrevIoTicks: ({})
    property var    _ioMountPoints: ({})
    property string _ioRVal:        "000B"
    property string _ioWVal:        "000B"
    property var    _ioDiskRates:   []
    property var    _ioReadHistory:  []
    property var    _ioWriteHistory: []

    // ── TMP state ──────────────────────────────────────────────────────────
    property int _tmpC: 0   // degrees Celsius

    // ── alert ──────────────────────────────────────────────────────────────
    property bool _alert: _cpuPct > 80 || _memPct > 80 || _tmpC > 85

    // ── helpers ────────────────────────────────────────────────────────────
    function _fmtIo(bps) {
        if (bps >= 1000000000) return String(Math.floor(bps / 1000000000)).padStart(3, "0") + "G"
        if (bps >= 1000000)    return String(Math.floor(bps / 1000000)).padStart(3, "0")    + "M"
        if (bps >= 1000)       return String(Math.floor(bps / 1000)).padStart(3, "0")       + "K"
        return String(Math.max(0, bps)).padStart(3, "0") + "B"
    }
    function _gb(kb) { return (kb / 1048576).toFixed(1) }

    // ── bar ────────────────────────────────────────────────────────────────
    Rectangle {
        anchors.fill: parent
        color: ma.containsMouse ? Theme.dim : "transparent"
    }

    RowLayout {
        id: row
        anchors.verticalCenter: parent.verticalCenter
        spacing: 0

        TlaLabel { text: "SYS"; color: root._alert ? Theme.alert : Theme.label_bg }

        StyledText {
            leftPadding: 6
            rightPadding: 4
            text: "C" + String(root._cpuPct).padStart(2, "0") +
                  " M" + String(root._memPct).padStart(2, "0") +
                  " T" + String(root._tmpC).padStart(2, "0") + "°"
            font.weight: Font.Bold
            verticalAlignment: Text.AlignVCenter
            color: root._alert ? Theme.alert : Theme.fg
        }
    }

    MouseArea {
        id: ma
        anchors.fill: parent
        hoverEnabled: true
        onEntered: popup.show(root)
        onExited: popup.hide()
    }

    // ── popup ──────────────────────────────────────────────────────────────
    BarPopup {
        id: popup
        barWindow: root.barWindow
        implicitWidth: 460
        implicitHeight: popupCol.implicitHeight + 12

        Column {
            id: popupCol
            anchors { top: parent.top; left: parent.left; topMargin: 6; leftMargin: 8 }
            spacing: 0
            width: popup.implicitWidth - 16

            // ── CPU ────────────────────────────────────────────────────────
            StyledText {
                text: "CPU  " + String(root._cpuPct).padStart(3, " ") + "%"
                color: root._cpuPct > 80 ? Theme.alert : Theme.label_bg
                font.weight: Font.Bold
            }

            Item { width: 1; height: 4 }

            SparkLine {
                width: popupCol.width
                height: 40
                values: root._cpuHistory
                maxVal: 100
                lineColor: Theme.label_bg
            }

            Item { width: 1; height: 6 }

            Grid {
                columns: 8
                spacing: 2

                Repeater {
                    model: root._coreValues

                    Rectangle {
                        required property var modelData
                        width: 54; height: 54
                        color: Theme.dim
                        clip: true

                        Rectangle {
                            anchors.bottom: parent.bottom
                            width: parent.width
                            height: parent.height * modelData.pct / 100
                            color: modelData.pct > 80 ? Theme.alert : Theme.label_bg
                            opacity: 0.5
                        }

                        Text {
                            anchors { centerIn: parent }
                            text: modelData.pct + "%"
                            font.family: Theme.font
                            font.pixelSize: Theme.fontSize
                            font.bold: true
                            color: Theme.fg
                        }
                    }
                }
            }

            Item { width: 1; height: 8 }

            // ── divider ────────────────────────────────────────────────────
            Rectangle { width: popupCol.width; height: 1; color: Theme.dim; opacity: 0.5 }
            Item { width: 1; height: 8 }

            // ── MEM ────────────────────────────────────────────────────────
            StyledText {
                text: "MEM  " + String(root._memPct).padStart(3, " ") + "%"
                color: root._memPct > 80 ? Theme.alert : Theme.label_bg
                font.weight: Font.Bold
            }

            Item { width: 1; height: 4 }

            SparkLine {
                width: popupCol.width
                height: 40
                values: root._memHistory
                maxVal: 100
                lineColor: Theme.label_bg
            }

            Item { width: 1; height: 6 }

            GridLayout {
                columns: 3
                columnSpacing: 10
                rowSpacing: 3

                property var s: root._memStats

                StyledText { text: "Used";      color: Theme.label_bg; font.weight: Font.Bold }
                Rectangle {
                    width: 100; height: 12
                    Layout.alignment: Qt.AlignVCenter
                    color: Theme.dim
                    Rectangle {
                        width: parent.parent.s.total > 0
                               ? parent.width * parent.parent.s.used / parent.parent.s.total : 0
                        height: parent.height
                        color: (parent.parent.s.used / parent.parent.s.total) > 0.8
                               ? Theme.alert : Theme.label_bg
                    }
                }
                StyledText { text: root._gb(parent.s.used) + " / " + root._gb(parent.s.total) + " GB"; color: Theme.fg }

                StyledText { text: "Avail";     color: Theme.fg }
                Item { width: 100; height: 12 }
                StyledText { text: root._gb(parent.s.available) + " GB"; color: Theme.fg }

                StyledText { text: "Buf/Cache"; color: Theme.fg }
                Item { width: 100; height: 12 }
                StyledText { text: root._gb(parent.s.buffers + parent.s.cached) + " GB"; color: Theme.fg }

                StyledText { text: "Swap";      color: Theme.fg }
                Rectangle {
                    width: 100; height: 12
                    Layout.alignment: Qt.AlignVCenter
                    color: Theme.dim
                    Rectangle {
                        width: parent.parent.s.swapTotal > 0
                               ? parent.width * parent.parent.s.swapUsed / parent.parent.s.swapTotal : 0
                        height: parent.height
                        color: Theme.label_bg
                    }
                }
                StyledText { text: root._gb(parent.s.swapUsed) + " / " + root._gb(parent.s.swapTotal) + " GB"; color: Theme.fg }
            }

            Item { width: 1; height: 8 }

            // ── divider ────────────────────────────────────────────────────
            Rectangle { width: popupCol.width; height: 1; color: Theme.dim; opacity: 0.5 }
            Item { width: 1; height: 8 }

            // ── IO ─────────────────────────────────────────────────────────
            StyledText {
                text: "IO   " + root._ioRVal + "r  " + root._ioWVal + "w"
                color: Theme.label_bg
                font.weight: Font.Bold
            }

            Item { width: 1; height: 4 }

            SparkLine {
                width: popupCol.width
                height: 30
                values: root._ioReadHistory
                lineColor: Theme.label_bg
            }

            Item { width: 1; height: 3 }

            SparkLine {
                width: popupCol.width
                height: 30
                values: root._ioWriteHistory
                lineColor: Theme.alert
            }

            Item { width: 1; height: 6 }

            GridLayout {
                columns: 6
                columnSpacing: 10
                rowSpacing: 3

                Repeater {
                    model: root._ioDiskRates

                    delegate: Item {
                        required property var modelData
                        required property int index
                        Layout.columnSpan: 6
                        implicitHeight: diskRow.implicitHeight
                        implicitWidth: diskRow.implicitWidth

                        Row {
                            id: diskRow
                            spacing: 10

                            StyledText {
                                text: modelData.name
                                width: 80
                                color: Theme.label_bg; font.weight: Font.Bold
                                verticalAlignment: Text.AlignVCenter
                            }
                            StyledText {
                                text: modelData.mounts.length > 0 ? modelData.mounts.join("  ") : "—"
                                width: 130
                                color: Theme.fg
                                elide: Text.ElideRight
                                verticalAlignment: Text.AlignVCenter
                            }
                            StyledText {
                                text: String(modelData.util).padStart(3, "0") + "%"
                                width: 40
                                horizontalAlignment: Text.AlignRight
                                color: modelData.util > 80 ? Theme.alert : Theme.fg
                                font.weight: modelData.util > 80 ? Font.Bold : Font.Normal
                                verticalAlignment: Text.AlignVCenter
                            }
                            StyledText {
                                text: "q:" + modelData.queue
                                width: 32
                                color: modelData.queue > 4 ? Theme.alert : Theme.dim
                                verticalAlignment: Text.AlignVCenter
                            }
                            StyledText {
                                text: root._fmtIo(modelData.read) + "r"
                                width: 50
                                horizontalAlignment: Text.AlignRight
                                color: Theme.fg
                                verticalAlignment: Text.AlignVCenter
                            }
                            StyledText {
                                text: root._fmtIo(modelData.write) + "w"
                                width: 50
                                horizontalAlignment: Text.AlignRight
                                color: Theme.fg
                                verticalAlignment: Text.AlignVCenter
                            }
                        }
                    }
                }
            }

            Item { width: 1; height: 8 }

            // ── divider ────────────────────────────────────────────────────
            Rectangle { width: popupCol.width; height: 1; color: Theme.dim; opacity: 0.5 }
            Item { width: 1; height: 8 }

            // ── TMP ────────────────────────────────────────────────────────
            StyledText {
                text: "TMP  " + root._tmpC + "°C"
                color: root._tmpC > 85 ? Theme.alert : Theme.fg
                font.weight: Font.Bold
            }

            Item { width: 1; height: 6 }
        }
    }

    // ── CPU data ───────────────────────────────────────────────────────────
    FileView {
        id: cpuFile
        path: "/proc/stat"
        onTextChanged: {
            const lines = text().split('\n')

            const parts = lines[0].trim().split(/\s+/).slice(1).map(Number)
            const idle  = parts[3] + parts[4]
            const total = parts.reduce((a, b) => a + b, 0)
            if (root._cpuPrevTotal >= 0) {
                const dt = total - root._cpuPrevTotal
                const di = idle  - root._cpuPrevIdle
                const pct = Math.max(0, Math.min(100,
                    dt > 0 ? Math.round((1 - di / dt) * 100) : 0))
                root._cpuPct = pct
                root._cpuHistory = root._cpuHistory.concat([pct]).slice(-60)
            }
            root._cpuPrevTotal = total
            root._cpuPrevIdle  = idle

            const coreLines = lines.filter(l => /^cpu\d/.test(l))
            const newIdle = [], newTotal = [], newVals = []
            for (let i = 0; i < coreLines.length; i++) {
                const cp = coreLines[i].trim().split(/\s+/).slice(1).map(Number)
                const ci = cp[3] + cp[4]
                const ct = cp.reduce((a, b) => a + b, 0)
                let pct = 0
                if (i < root._prevCoreTotal.length) {
                    const dt = ct - root._prevCoreTotal[i]
                    const di = ci - root._prevCoreIdle[i]
                    pct = dt > 0 ? Math.round((1 - di / dt) * 100) : 0
                    pct = Math.max(0, Math.min(100, pct))
                }
                newIdle.push(ci); newTotal.push(ct)
                newVals.push({ label: "cpu" + i, pct })
            }
            root._prevCoreIdle  = newIdle
            root._prevCoreTotal = newTotal
            root._coreValues    = newVals
        }
    }

    // ── MEM data ───────────────────────────────────────────────────────────
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

            root._memStats = { total, used, available, buffers, cached, swapTotal, swapUsed: swapTotal - swapFree }
            const pct = total > 0 ? Math.round(used / total * 100) : 0
            root._memPct = pct
            root._memHistory = root._memHistory.concat([pct]).slice(-60)
        }
    }

    // ── IO data ────────────────────────────────────────────────────────────
    FileView {
        id: mountsFile
        path: "/proc/mounts"
        onTextChanged: {
            const mp = {}
            text().split('\n').forEach(line => {
                const p = line.trim().split(/\s+/)
                if (p.length < 2 || !p[0].startsWith('/dev/')) return
                const dev  = p[0].substring(5)
                const disk = dev.replace(/p\d+$/, '').replace(/\d+$/, '')
                if (!mp[disk]) mp[disk] = []
                mp[disk].push(p[1])
            })
            root._ioMountPoints = mp
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

                newR[name] = secR; newW[name] = secW; newT[name] = ioTicks

                let read = 0, write = 0, util = 0
                if (name in root._ioPrevRead) {
                    read  = Math.max(0, (secR    - root._ioPrevRead[name])    * 512)
                    write = Math.max(0, (secW    - root._ioPrevWrite[name])   * 512)
                    util  = Math.min(100, Math.max(0,
                                Math.round((ioTicks - root._ioPrevIoTicks[name]) / 10)))
                    totalRead  += read
                    totalWrite += write
                }
                rates.push({ name, read, write, util, queue,
                             mounts: root._ioMountPoints[name] ?? [] })
            })

            root._ioRVal         = root._fmtIo(totalRead)
            root._ioWVal         = root._fmtIo(totalWrite)
            root._ioPrevRead     = newR
            root._ioPrevWrite    = newW
            root._ioPrevIoTicks  = newT
            root._ioDiskRates    = rates
            root._ioReadHistory  = root._ioReadHistory.concat([totalRead]).slice(-60)
            root._ioWriteHistory = root._ioWriteHistory.concat([totalWrite]).slice(-60)
        }
    }

    // ── TMP data ───────────────────────────────────────────────────────────
    FileView {
        id: tmpFile
        path: Config.tempInput
        onTextChanged: {
            root._tmpC = Math.round(parseInt(text().trim()) / 1000)
        }
    }

    // ── timers ─────────────────────────────────────────────────────────────
    Timer {
        interval: 1000
        running: true; repeat: true; triggeredOnStart: true
        onTriggered: { cpuFile.reload(); diskstats.reload(); mountsFile.reload() }
    }

    Timer {
        interval: 2000
        running: true; repeat: true; triggeredOnStart: true
        onTriggered: { memFile.reload(); tmpFile.reload() }
    }
}
