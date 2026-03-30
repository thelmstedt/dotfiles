// CpuWidget.qml — native /proc/stat diff, no subprocess
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

    property real _prevIdle: -1
    property real _prevTotal: -1
    property string _value: "  0"
    property var _history: []

    property var _prevCoreIdle: []
    property var _prevCoreTotal: []
    property var _coreFreqs: []
    property var _coreValues: []  // [{label, pct, freq}, ...]

    Rectangle {
        anchors.fill: parent
        color: ma.containsMouse ? Theme.dim : "transparent"
    }

    RowLayout {
        id: row
        anchors.verticalCenter: parent.verticalCenter
        spacing: 0

        TlaLabel { text: "CPU" }

        StyledText {
            leftPadding: 6
            rightPadding: 4
            text: root._value + "%"
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

            Column {
                spacing: 3

                Repeater {
                    model: root._coreValues

                    Row {
                        required property var modelData
                        spacing: 6

                        StyledText {
                            text: modelData.label
                            width: 36
                            color: Theme.fg
                            verticalAlignment: Text.AlignVCenter
                        }

                        Rectangle {
                            width: 100
                            height: 12
                            anchors.verticalCenter: parent.verticalCenter
                            color: Theme.dim

                            Rectangle {
                                width: parent.width * modelData.pct / 100
                                height: parent.height
                                color: modelData.pct > 80 ? Theme.alert : Theme.label_bg
                            }
                        }

                        StyledText {
                            text: String(modelData.pct).padStart(3) + "%"
                            color: Theme.fg
                            verticalAlignment: Text.AlignVCenter
                        }

                        StyledText {
                            text: modelData.freq > 0 ? (modelData.freq / 1000).toFixed(2) + "G" : ""
                            color: Theme.dim
                            verticalAlignment: Text.AlignVCenter
                        }
                    }
                }
            }
        }
    }

    FileView {
        id: statFile
        path: "/proc/stat"
        onTextChanged: {
            const lines = text().split('\n')

            // Aggregate
            const parts = lines[0].trim().split(/\s+/).slice(1).map(Number)
            const idle  = parts[3] + parts[4]
            const total = parts.reduce((a, b) => a + b, 0)
            if (root._prevTotal >= 0) {
                const dt = total - root._prevTotal
                const di = idle  - root._prevIdle
                const pct = Math.max(0, Math.min(100,
                    dt > 0 ? Math.round((1 - di / dt) * 100) : 0))
                root._value = String(pct).padStart(3)
                root._history = root._history.concat([pct]).slice(-60)
            }
            root._prevTotal = total
            root._prevIdle  = idle

            // Per-core
            const coreLines = lines.filter(l => /^cpu\d/.test(l))
            const newIdle = [], newTotal = [], newVals = []
            const freqs = root._coreFreqs
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
                newIdle.push(ci)
                newTotal.push(ct)
                newVals.push({ label: "cpu" + i, pct: pct, freq: freqs[i] ?? 0 })
            }
            root._prevCoreIdle = newIdle
            root._prevCoreTotal = newTotal
            root._coreValues = newVals
        }
    }

    FileView {
        id: cpuinfoFile
        path: "/proc/cpuinfo"
        onTextChanged: {
            const freqs = []
            text().split('\n').forEach(line => {
                const m = line.match(/^cpu MHz\s*:\s*([\d.]+)/)
                if (m) freqs.push(Math.round(parseFloat(m[1])))
            })
            root._coreFreqs = freqs
        }
    }

    Timer {
        interval: 1000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: { statFile.reload(); cpuinfoFile.reload() }
    }
}
