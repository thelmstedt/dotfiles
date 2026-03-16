// MprisWidget.qml
import QtQuick
import QtQuick.Layouts
import Quickshell.Services.Mpris
import qs.service
import qs.component

Item {
    id: root
    implicitWidth: row.implicitWidth
    implicitHeight: Theme.barHeight
    visible: Mpris.players.values.length > 0

    property var player: Mpris.players.values.length > 0 ? Mpris.players.values[0] : null
    property var limit: 50

    property string statusLabel: {
        if (!player) return "STP"
        switch (player.playbackState) {
            case 1: return "PLY"  // Playing
            case 2: return "HLT"  // Paused
            default: return "STP"
        }
    }

    property string trackText: {
        if (!player) return ""
        const fields = {
                artist: player.trackArtist || "",
                album: player.trackAlbum || "",
                title: player.trackTitle || "",
                tracknumber: player.metadata?.["xesam:trackNumber"]?.toString() || ""
        }
        const order = ["artist", "album", "tracknumber", "title"]
        const priority = ["title", "artist", "album", "tracknumber"]
        // priority[0] = most important (last to drop)
        // build from order, drop lowest priority until fits
        let active = order.filter(k => fields[k])
        while (active.length > 1) {
            const candidate = active.map(k => fields[k]).join(" – ")
            if (candidate.length <= limit) break
            // find lowest priority field still active
            const toDrop = [...priority].reverse().find(k => active.includes(k))
            active = active.filter(k => k !== toDrop)
        }
        const full = active.map(k => fields[k]).join(" – ")
        return full.length > limit ? full.substring(0, limit - 3) + "..." : full
    }

    Rectangle {
        anchors.fill: parent
        color: ma.containsMouse ? Theme.dim : "transparent"
    }

    RowLayout {
        id: row
        anchors.verticalCenter: parent.verticalCenter
        spacing: 0

        TlaLabel { text: root.statusLabel }

        StyledText {
            leftPadding: 6
            rightPadding: 10
            text: root.trackText
            font.weight: Font.Bold
            verticalAlignment: Text.AlignVCenter
            visible: root.trackText.length > 0
        }
    }

    MouseArea {
        id: ma
        anchors.fill: parent
        hoverEnabled: true
        onClicked: if (root.player?.canTogglePlaying) root.player.togglePlaying()
    }
}
