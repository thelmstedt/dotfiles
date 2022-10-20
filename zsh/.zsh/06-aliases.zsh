alias ls='ls --color=auto'
if [[ -r "/usr/bin/exa" ]] ; then
  alias l="exa"
  alias ll="exa -l --git"
else
  alias l="ls"
  alias ll="ls -l"
fi

alias trim="sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//'"
alias lower="tr '[:upper:]' '[:lower:]'"
alias upper="tr '[:lower:]' '[:upper:]'"
# shellcheck disable=SC2154
alias sumcolumn="sed '\$a\' | tr '\n' '+' | sed 's/+$/\n/' | bc"
alias pipesize="wc -c |  numfmt --to iec --format '%8.4f'"


# git
alias g="git"
alias gs="git status --short"
alias e="emacsclient -t"

# terraform
alias tf="terraform"

# k8s
alias k="kubectl"
function ke() {
    echo kubectl exec --stdin --tty -n "$1" "$2" -- "${@:3}"
    kubectl exec --stdin --tty -n "$1" "$2" -- "${@:3}"
}
# for TmvTail
function ktt() {
    echo ke "$1" "$2" tail -f /tmv/ceeqtm/logs/django/exceptions.log "${@:2}"
    ke "$1" "$2" tail -f /tmv/ceeqtm/logs/django/exceptions.log "${@:2}"
}

function de() {
  echo docker exec -it "$1" "$2"
  docker exec -it "$1" "$2"
}


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
