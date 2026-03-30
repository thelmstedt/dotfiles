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

    property var _prevRead:  ({})
    property var _prevWrite: ({})
    property string _rVal: "0B"
    property string _wVal: "0B"
    property var _diskRates: []  // [{name, read, write}, ...]

    function _fmt(bps) {
        if (bps >= 1000000000) return String(Math.floor(bps / 1000000000)).padStart(3, "0") + "G"
        if (bps >= 1000000)    return String(Math.floor(bps / 1000000)).padStart(3, "0") + "M"
        if (bps >= 1000)       return String(Math.floor(bps / 1000)).padStart(3, "0") + "K"
        return String(Math.max(0, bps)).padStart(3, "0") + "B"
    }

    // Measure the widest possible string to lock element width
    TextMetrics {
        id: tm
        font.family: Theme.font
        font.pixelSize: Theme.fontSize
        font.weight: Font.Bold
        text: "000Gr"
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
            Layout.preferredWidth: tm.boundingRect.width + leftPadding
            horizontalAlignment: Text.AlignRight
            text: root._rVal + "r"
            font.weight: Font.Bold
            verticalAlignment: Text.AlignVCenter
        }

        StyledText {
            leftPadding: 4
            rightPadding: 4
            Layout.preferredWidth: tm.boundingRect.width + leftPadding + rightPadding
            horizontalAlignment: Text.AlignRight
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
        implicitWidth: col.implicitWidth + 16
        implicitHeight: col.implicitHeight + 12

        Column {
            id: col
            anchors { top: parent.top; left: parent.left; topMargin: 6; leftMargin: 8 }
            spacing: 3

            Repeater {
                model: root._diskRates

                Row {
                    required property var modelData
                    spacing: 6

                    StyledText {
                        text: modelData.name
                        width: 72
                        color: Theme.label_bg
                        font.weight: Font.Bold
                        verticalAlignment: Text.AlignVCenter
                    }

                    StyledText {
                        text: root._fmt(modelData.read) + "r"
                        width: tm.boundingRect.width
                        horizontalAlignment: Text.AlignRight
                        color: Theme.fg
                        verticalAlignment: Text.AlignVCenter
                    }


                    StyledText {
                        text: root._fmt(modelData.write) + "w"
                        width: tm.boundingRect.width
                        horizontalAlignment: Text.AlignRight
                        color: Theme.fg
                        verticalAlignment: Text.AlignVCenter
                    }
                }
            }
        }
    }

    FileView {
        id: diskstats
        path: "/proc/diskstats"
        onTextChanged: {
            let totalRead = 0, totalWrite = 0
            const newR = {}, newW = {}
            const rates = []

            text().split('\n').forEach(line => {
                const p = line.trim().split(/\s+/)
                if (p.length < 14) return
                const name = p[2]
                if (!/^(sd[a-z]|hd[a-z]|nvme\d+n\d+|vd[a-z]|xvd[a-z])$/.test(name)) return

                const secR = parseInt(p[5])
                const secW = parseInt(p[9])
                newR[name] = secR
                newW[name] = secW

                let read = 0, write = 0
                if (name in root._prevRead) {
                    read  = Math.max(0, (secR - root._prevRead[name])  * 512)
                    write = Math.max(0, (secW - root._prevWrite[name]) * 512)
                    totalRead  += read
                    totalWrite += write
                }
                rates.push({ name, read, write })
            })

            root._rVal      = root._fmt(totalRead)
            root._wVal      = root._fmt(totalWrite)
            root._prevRead  = newR
            root._prevWrite = newW
            root._diskRates = rates
        }
    }

    Timer {
        interval: 1000
        running: true
        repeat: true
        triggeredOnStart: true
        onTriggered: diskstats.reload()
    }
}
