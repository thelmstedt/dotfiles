// TlaLabel.qml — coloured badge replacing pango span hacks
import QtQuick
import qs.service


Rectangle {
    id: root
    required property string text

    color: Theme.label_bg
    implicitWidth: lbl.implicitWidth + 8
    implicitHeight: parent.height

    Text {
        id: lbl
        anchors.centerIn: parent
        text: root.text
        color: Theme.label_fg
        font.family: Theme.font
        font.pixelSize: Theme.fontSize
        font.bold: true
    }
}
