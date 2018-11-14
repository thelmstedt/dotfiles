#!/bin/sh

dbus-send --print-reply --dest=org.mpris.clementine /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause
