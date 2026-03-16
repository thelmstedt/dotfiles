pragma ComponentBehavior: Bound

import QtQuick
import qs.service


Text {
    id: root
    renderType: Text.NativeRendering
    textFormat: Text.PlainText
    color: Theme.fg
    font.family: Theme.font
    font.pixelSize: Theme.fontSize
    font.weight: Font.Normal
}