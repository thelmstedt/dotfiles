import QtQuick
import Quickshell
import Quickshell.Services.SystemTray
import qs.service


Row {
    id: root
    required property var barWindow
    spacing: 4
    leftPadding: 6
    rightPadding: 6

    Repeater {
        model: SystemTray.items.values

        delegate: Item {
            id: trayItem
            required property SystemTrayItem modelData
            implicitWidth: 22
            implicitHeight: 22

            Image {
                anchors.fill: parent
                fillMode: Image.PreserveAspectFit
                smooth: true

                property string _raw: trayItem.modelData.icon
                property string _resolved: {
                    const q = _raw.indexOf("?path=")
                    if (q === -1) return _raw
                    const path = _raw.substring(q + 6)
                    const name = _raw.substring(0, q).split("/").pop()
                    return "file://" + path + "/" + name + ".png"
                }

                source: _resolved
                onStatusChanged: {
                    if (status !== Image.Error) return
                    if (source === _resolved)
                        source = _resolved.slice(0, -4) + ".svg"
                    else if (source !== _raw)
                        source = _raw
                }
            }

            QsMenuAnchor {
                id: menuAnchor
                menu: trayItem.modelData.menu
                anchor.window: root.barWindow
                anchor.rect: {
                    const pos = trayItem.mapToItem(root.barWindow.contentItem, 0, 0)
                    return Qt.rect(pos.x, pos.y, trayItem.width, trayItem.height)
                }
                anchor.edges: Edges.Bottom
                anchor.gravity: Edges.Bottom
                anchor.adjustment: PopupAdjustment.Flip | PopupAdjustment.Slide
            }

            MouseArea {
                anchors.fill: parent
                acceptedButtons: Qt.LeftButton | Qt.RightButton
                onClicked: (mouse) => {
                    if (mouse.button === Qt.LeftButton && !trayItem.modelData.onlyMenu)
                        trayItem.modelData.activate()
                    else
                        menuAnchor.open()
                }
            }
        }
    }
}
