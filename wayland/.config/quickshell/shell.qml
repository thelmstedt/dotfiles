//@ pragma UseQApplication
//@ pragma NativeTextRendering
//@ pragma IconTheme WhiteSur-dark
//@ pragma Env QS_NO_RELOAD_POPUP=1

import QtQuick
import Quickshell


ShellRoot {
    Variants {
        model: Quickshell.screens
        delegate: Bar {
            required property var modelData
            screen: modelData
        }
    }



}

