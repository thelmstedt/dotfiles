import QtQuick
import qs.service
import qs.component


StyledText {
    id: root
    leftPadding: 10
    rightPadding: 10
    verticalAlignment: Text.AlignVCenter
    font.weight: Font.Bold

    property var now: new Date()
    text: Qt.formatDateTime(now, "yyyy-MM-dd HH:mm:ss")

    Timer {
        interval: 1000
        running: true
        repeat: true
        onTriggered: root.now = new Date()
    }
}