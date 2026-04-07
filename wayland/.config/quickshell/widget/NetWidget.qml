// NetWidget.qml — combined upload + download with popup sparklines
import QtQuick
import QtQuick.Layouts
import Quickshell.Io
import qs.service
import qs.component

Item {
    id: root
    required property var barWindow
    property string iface: Config.netIface

    implicitWidth: row.implicitWidth
    implicitHeight: Theme.barHeight

    property real _prevTx: -1
    property real _prevRx: -1
    property string _uplVal: "000B"
    property string _dnlVal: "000B"
    property var _uplHistory: []
    property var _dnlHistory: []
    property var _filterBelowBytes: 1000

    function _fmt(bps) {
        if (bps < _filterBelowBytes) return "----"
        if (bps >= 1000000000) return String(Math.floor(bps / 1000000000)).padStart(3, "0") + "G"
        if (bps >= 1000000)    return String(Math.floor(bps / 1000000)).padStart(3, "0")    + "M"
        if (bps >= 1000)       return String(Math.floor(bps / 1000)).padStart(3, "0")       + "K"
        return String(Math.max(0, bps)).padStart(3, "0") + "B"
    }

    // ── bar ────────────────────────────────────────────────────────────────
    Rectangle {
        anchors.fill: parent
        color: ma.containsMouse ? Theme.dim : "transparent"
    }

    RowLayout {
        id: row
        anchors.verticalCenter: parent.verticalCenter
        spacing: 0

        TlaLabel { text: "NET" }

        StyledText {
            leftPadding: 6
            rightPadding: 4
            text: "U" + root._uplVal + " D" + root._dnlVal
            font.weight: Font.Bold
            verticalAlignment: Text.AlignVCenter
        }
    }

    MouseArea {
        id: ma
        anchors.fill: parent
        hoverEnabled: true
        onEntered: popup.show(root)
        onExited:  popup.hide()
    }

    // ── popup ──────────────────────────────────────────────────────────────
    BarPopup {
        id: popup
        barWindow: root.barWindow
        implicitWidth: 360
        implicitHeight: popupCol.implicitHeight + 12

        Column {
            id: popupCol
            anchors { top: parent.top; left: parent.left; topMargin: 6; leftMargin: 8 }
            spacing: 0
            width: popup.implicitWidth - 16

            StyledText {
                text: "UPL  " + root._uplVal + "/s"
                color: Theme.label_bg
                font.weight: Font.Bold
            }

            Item { width: 1; height: 4 }

            SparkLine {
                width: popupCol.width
                height: 40
                values: root._uplHistory
                lineColor: Theme.label_bg
            }

            Item { width: 1; height: 8 }

            StyledText {
                text: "DNL  " + root._dnlVal + "/s"
                color: Theme.label_bg
                font.weight: Font.Bold
            }

            Item { width: 1; height: 4 }

            SparkLine {
                width: popupCol.width
                height: 40
                values: root._dnlHistory
                lineColor: Theme.alert
            }

            Item { width: 1; height: 6 }
        }
    }

    // ── data ───────────────────────────────────────────────────────────────
    FileView {
        id: txFile
        path: "/sys/class/net/" + root.iface + "/statistics/tx_bytes"
        onTextChanged: {
            const now = parseInt(text().trim())
            if (root._prevTx >= 0) {
                const bps = Math.max(0, now - root._prevTx)
                root._uplVal = root._fmt(bps)
                root._uplHistory = root._uplHistory.concat([bps]).slice(-60)
            }
            root._prevTx = now
        }
    }

    FileView {
        id: rxFile
        path: "/sys/class/net/" + root.iface + "/statistics/rx_bytes"
        onTextChanged: {
            const now = parseInt(text().trim())
            if (root._prevRx >= 0) {
                const bps = Math.max(0, now - root._prevRx)
                root._dnlVal = root._fmt(bps)
                root._dnlHistory = root._dnlHistory.concat([bps]).slice(-60)
            }
            root._prevRx = now
        }
    }

    Timer {
        interval: 1000
        running: true; repeat: true; triggeredOnStart: true
        onTriggered: { txFile.reload(); rxFile.reload() }
    }
}
