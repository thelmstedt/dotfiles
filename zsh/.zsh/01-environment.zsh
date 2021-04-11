##
## timestamps
##
#HIST_STAMPS=yyyy/mm/dd

##
## paths
##
export PATH=/usr/local/bin:$HOME/bin:$HOME/.local/bin:$PATH
[[ -s "$HOME/bin" ]] && PATH=$PATH:$HOME/bin
[[ -s "$HOME/.local/bin" ]] && PATH=$PATH:$HOME/.local/bin


##
## edit / view
##
export EDITOR=nano
export GIT_EDITOR=nano
export PAGER=less
export LESS='-F -g -i -M -R -S -w -z-4'


##
## manpages
##

export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;38;5;74m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[30;43m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[04;38;5;146m'

##
## language
##
export LC_COLLATE=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LC_MESSAGES=en_US.UTF-8
export LC_MONETARY=en_US.UTF-8
export LC_NUMERIC=en_US.UTF-8
export LC_TIME=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export LESSCHARSET=utf-8


# Python
[[ -s "$HOME/.pythonrc.py" ]] && export PYTHONSTARTUP="$HOME/.pythonrc.py"

##
## java fixes
##
export _JAVA_AWT_WM_NONREPARENTING=1
#Java
export MAVEN_OPTS=" -Xmx2g -XX:MaxPermSize=256m -Dmaven.artifact.threads=20 -Djava.awt.headless=true"
export ES_JAVA_OPTS='-Xmx4g -Xms4g' #ES5
export GRADLE_USER_HOME="$HOME/.gradle"

##
## no mosh titles
##
export MOSH_TITLE_NOPREFIX=1

# Machine Specific
[[ -s "$HOME/.private" ]] && source "$HOME/.private"
[[ -s "$HOME/.private_env" ]] && source "$HOME/.private_env"
