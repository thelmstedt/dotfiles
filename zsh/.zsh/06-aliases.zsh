if [[ "$OS" == "mac" ]]; then
  # missing commands in osx
  alias tac="tail -r"
fi

alias ls='ls --color=auto'
if [[ -r "/usr/bin/exa" ]] ; then
  alias l="exa"
  alias ll="exa -l --git"
else
  alias l="ls"
  alias ll="ls -l"
fi

alias startx=Hyprland
alias trim="sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//'"
alias lower="tr '[:upper:]' '[:lower:]'"
alias upper="tr '[:lower:]' '[:upper:]'"
# shellcheck disable=SC2154
alias sumcolumn="sed '\$a\' | tr '\n' '+' | sed 's/+$/\n/' | bc"

# git
alias g="git"
alias gs="git status --short"
alias e="emacsclient -t"

# terraform
alias tf="terraform"

# k8s
alias k="kubectl"
function ke() {
    local pod namespace
    zparseopts -D n:=namespace
    namespace=${namespace[2]:-default}
    pod="$1"
    shift
    set -x
    kubectl exec --stdin --tty -n "$namespace" "$pod" -- "$@"
}

function de() {
 docker exec -it "$(docker ps | grep $1 | awk '{print $1}')" "$2"
}
alias dc="docker compose"
alias vpndo='nsenter --target $(docker inspect --format "{{.State.Pid}}" vpn-vpn-1) --net --setuid $(id -u)'


alias aws-display="~/work/aws-pricing/.venv/bin/python ~/work/aws-pricing/aws-display.py"
alias git-recent="~/bin/.venv/bin/python ~/bin/git-recent.py"


# Maven
alias -g md='MAVEN_OPTS="-Xdebug -Xnoagent -Djava.compiler=NONE -Xrunjdwp:transport=dt_socket,address=8000,server=y,suspend=n $MAVEN_OPTS " '
alias m='mvn'
alias mi="mvn install"
alias mci="mvn clean install"
alias mdt="mvn dependency:tree"

# Gradle
alias -g gd='GRADLE_OPTS="-Xdebug -Xnoagent -Djava.compiler=NONE -Xrunjdwp:transport=dt_socket,address=5005,server=y,suspend=y $GRADLE_OPTS " '
alias -g gbd='GRADLE_OPTS="-Xdebug -Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=5005 $GRADLE_OPTS"'
alias -g nt="-xcheck -xtest"
alias r='gradle'
alias gw='$(git rev-parse --show-toplevel)/gradlew'

# desk
alias deskup="uvx linak-controller --mac-address AD739686-0D5B-06A9-5D81-68385BD54DC4 --move-to 1150"
alias deskdown="uvx linak-controller --mac-address AD739686-0D5B-06A9-5D81-68385BD54DC4 --move-to 730"


# wayland
alias postman="postman --ozone-platform-hint=auto --enable-features=WaylandWindowDecorations"
alias spotify="spotify --ozone-platform-hint=auto --enable-features=WaylandWindowDecorations"
