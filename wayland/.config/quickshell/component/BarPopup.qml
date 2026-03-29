// component/BarPopup.qml — anchored hover popup for bar widgets
// Usage: call show(anchorItem) / hide() from the widget's MouseArea
import QtQuick
import Quickshell
import qs.service

PopupWindow {
    id: root
    required property var barWindow

    visible: false
    color: Theme.bg

    anchor.window: barWindow
    anchor.edges: Edges.Bottom
    anchor.gravity: Edges.Bottom
    anchor.adjustment: PopupAdjustment.Flip | PopupAdjustment.Slide

    // Compute position at show()-time (not as a binding) so we get the
    // real layout position, not the unresolved x=0 from early init.
    function show(item) {
        const pos = item.mapToItem(barWindow.contentItem, 0, 0)
        anchor.rect = Qt.rect(pos.x, pos.y, item.width, item.height)
        visible = true
    }

    function hide() { visible = false }
}
