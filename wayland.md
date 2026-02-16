# wayland

## Application Switcher - Walker / Elephant

Deps

    yay -S walker-bin \
        elephant-websearch-bin \
        elephant-unicode-bin \
        elephant-todo-bin \
        elephant-symbols-bin \
        elephant-runner-bin \
        elephant-providerlist-bin \
        elephant-menus-bin \
        elephant-files-bin \
        elephant-desktopapplications-bin \
        elephant-clipboard-bin \
        elephant-calc-bin \
        elephant-archlinuxpkgs-bin \
        elephant-bluetooth-bin \
        elephant-windows-bin \
        elephant-snippets-bin \
        elephant-niriactions-bin \
        elephant-nirisessions-bin \
        elephant-bookmarks-bin \
        elephant-1password-bin \
        elephant-dnfpackages-bin \
        elephant-bitwarden-bin \
        libqalculate


Service

    ❯ cat /home/tim/.config/systemd/user/walker.service;
    [Unit]
    Description=Walker
    After=graphical-session.target
    
    [Service]
    Type=simple
    ExecStart=walker --gapplication-service
    Restart=on-failure
    
    [Install]
    WantedBy=graphical-session.target   

