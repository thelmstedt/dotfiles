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

    property var lastPlayingPlayer: null

    // Reactively picks the playing player; accessing playbackState in the loop
    // creates a binding dependency on every player's state, so this re-evaluates
    // whenever any player starts/stops playing.
    property var player: {
        const values = Mpris.players.values
        for (let i = 0; i < values.length; i++) {
            if (values[i].playbackState === 1) return values[i]
        }
        // Nothing playing — fall back to last known playing player if still around
        if (lastPlayingPlayer) {
            for (let i = 0; i < values.length; i++) {
                if (values[i] === lastPlayingPlayer) return lastPlayingPlayer
            }
        }
        return values.length > 0 ? values[0] : null
    }

    onPlayerChanged: {
        if (player?.playbackState === 1) lastPlayingPlayer = player
    }

    // Catch state changes on the same player (no player change, but playbackState change)
    Connections {
        target: root.player
        function onPlaybackStateChanged() {
            if (root.player?.playbackState === 1) root.lastPlayingPlayer = root.player
        }
    }

    property var limit: 150

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
        const sep = { album: " | ", artist: " - ", tracknumber: ". " }

        let active = order.filter(k => fields[k])
        while (active.length > 1) {
            const candidate = active.map((k, i) => fields[k] + (i < active.length - 1 ? sep[k] : "")).join("")
            if (candidate.length <= limit) break
            const toDrop = [...priority].reverse().find(k => active.includes(k))
            active = active.filter(k => k !== toDrop)
        }
        const full = active.map((k, i) => fields[k] + (i < active.length - 1 ? sep[k] : "")).join("")
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
            rightPadding: 4
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
