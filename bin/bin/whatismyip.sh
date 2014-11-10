#!/usr/bin/sh


function doit {
    (wget -O - http://ip.tupeux.com | tail) > ~/.myip
}

[[ ! -f ~/.myip ]] && doit

if test "`find ~/.myip -mmin +1440`"; then
    rm ~/.myip
    doit
fi


