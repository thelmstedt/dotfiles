# Packages
AddPackage a2jmidid # A daemon for exposing legacy ALSA sequencer applications in JACK MIDI system.
AddPackage alsa-utils # Advanced Linux Sound Architecture - Utilities
AddPackage jack2 # C++ version of the JACK low-latency audio server for multi-processor machines
AddPackage paprefs # Configuration dialog for PulseAudio
AddPackage pasystray # PulseAudio system tray (a replacement for padevchooser)
AddPackage pavucontrol # PulseAudio Volume Control
AddPackage pulseaudio # A featureful, general-purpose sound server
AddPackage qjackctl # A Qt front-end for the JACK low-latency audio server
AddPackage realtime-privileges # Realtime privileges for users

# AUR
AddPackage --foreign vcvrack-bin # Open-source virtual modular synthesizer

# Files
CopyFile /etc/pulse/daemon.conf
CopyFile /etc/pulse/default.pa

# Services
CreateLink /etc/systemd/user/sockets.target.wants/pulseaudio.socket /usr/lib/systemd/user/pulseaudio.socket
