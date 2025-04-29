# Setup

Manage AUR with `yay`

    git clone https://aur.archlinux.org/yay-git.git
    makepkg -si

Manage machine setup with `aconfmgr` and user setup with `stow`

    yay -S stow aconfmgr

`stow` what you want into your $HOME

    stow zsh bin machine_base x11 prime

Provided we've stowed `~/.config/aconfmgr` in the previous step - we can restore our machine config

    aconfmgr apply

# App Notes

## wayland

### readline mappings

Use keyd to have global readline ctrl-a, ctrl-e. also remaps ctrl+alt to recover normal behaviour

    # cat /etc/keyd/default.conf
    [ids]
    *
    
    [control]
    a = home
    e = end
    
    [control+alt]
    a = control+alt+a
    e = control+alt+e

### keyring

We handle ssh keys and secrets separately, this starts 2 gnome-keyring-daemons but they are running different modules so
this seems fine

    # handles ssh keys
    systemctl --user enable gcr-ssh-agent.socket --now
    
    # handles certs/passwords
    systemctl --user enable gnome-keyring-daemon.service --now 

Automatic unlock on login is a pain, had to add

    auth       optional     pam_gnome_keyring.so
    session    optional     pam_gnome_keyring.so auto_start

To `/etc/pam.d/{login,system-login}`, and just the auth bit to `/etc/pam.d/passwd`

Make sure to export the socket for `ssh-add` to work (this is in our zshrc)

    export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/gcr/ssh"

## Firefox

Put `./extrabits/firefox/{user.js,userChrome.css}` in their homes in your firefox profile

## IntelliJ

Enable wayland
    
    -Dawt.toolkit.name=WLToolkit

### Python debugging slow?

As of `2024.3` python debugging has MASSIVELY slowed down. Edit this registry value

    python.debug.low.impact.monitoring.api=False


### Focus follows mouse

[Fix described here](https://youtrack.jetbrains.com/issue/IDEA-112015#comment=27-1324403)

In the registry  (Ctrl+Shift+A and look for "Registry...")

- disable `allow.dialog.based.popup`
- enable `focus.follows.mouse.workarounds`

## cuda

    yay -S cuda cudnn cuSPARSELt nccl 

