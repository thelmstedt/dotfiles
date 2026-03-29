import QtQuick
import Quickshell.Hyprland
import qs.service
import qs.component

StyledText {
    required property var monitor
    leftPadding: 4
    rightPadding: 4
    font.weight: Font.Bold
    verticalAlignment: Text.AlignVCenter
    text: {
        const t = Hyprland.toplevels.values.find(
            tl => tl.activated && tl.monitor?.id === monitor?.id
        )
        return t?.title ?? ""
    }
}
