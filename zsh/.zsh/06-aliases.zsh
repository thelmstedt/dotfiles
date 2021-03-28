# Project aliases
alias w="cd $HOME/work"
alias b="cd $HOME/work/ipa-monorepo"
alias c="cd $HOME/work/fetch-admin"

alias ls='ls --color=auto'

alias l="exa"
alias ll="exa -l --git"
alias trim="sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//'"

# git
alias g="git"
alias gs="git status --short"
alias e="emacsclient -t"

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
alias rbr='gradle bootRun'


