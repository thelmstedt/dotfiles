# Packages
AddPackage a2jmidid # A daemon for exposing legacy ALSA sequencer applications in JACK MIDI system.
AddPackage alsa-utils # Advanced Linux Sound Architecture - Utilities
AddPackage paprefs # Configuration dialog for PulseAudio
AddPackage pasystray # PulseAudio system tray (a replacement for padevchooser)
AddPackage pavucontrol # PulseAudio Volume Control
AddPackage pipewire-alsa # Low-latency audio/video router and processor - ALSA configuration
AddPackage pipewire-jack # Low-latency audio/video router and processor - JACK support
AddPackage pipewire-pulse # Low-latency audio/video router and processor - PulseAudio replacement
AddPackage qjackctl # A Qt front-end for the JACK low-latency audio server
AddPackage realtime-privileges # Realtime privileges for users

# AUR
AddPackage --foreign vcvrack-bin # Open-source virtual modular synthesizer

# Files
CreateLink /etc/systemd/user/pipewire.service.wants/pipewire-media-session.service /usr/lib/systemd/user/pipewire-media-session.service
CreateLink /etc/systemd/user/pipewire-session-manager.service /usr/lib/systemd/user/pipewire-media-session.service
CreateLink /etc/systemd/user/sockets.target.wants/pipewire-pulse.socket /usr/lib/systemd/user/pipewire-pulse.socket
CreateLink /etc/systemd/user/sockets.target.wants/pipewire.socket /usr/lib/systemd/user/pipewire.socket
