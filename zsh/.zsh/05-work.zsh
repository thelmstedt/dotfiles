# Python
[[ -s "$HOME/.pythonrc.py" ]] && export PYTHONSTARTUP="$HOME/.pythonrc.py"
[ -f /opt/miniconda3/etc/profile.d/conda.sh ] && source /opt/miniconda3/etc/profile.d/conda.sh
export PYTHONIOENCODING=UTF-8

alias w="cd $HOME/work"
alias b="cd $HOME/work/tmv"

#
# AWS
#
alias -g apatms='AWS_PROFILE=atms'
alias -g apwa='AWS_PROFILE=wa-dev'

alias -g apcd='AWS_PROFILE=clarivate'
alias -g apcp='AWS_PROFILE=clarivate-prod'

function aws_login () {
  set -x
  apcd BROWSER=/usr/bin/chromium aws sso login
  apcp BROWSER=/usr/bin/chromium aws sso login
  apcp BROWSER=/usr/bin/chromium aws sts get-caller-identity
  apcd BROWSER=/usr/bin/chromium aws sts get-caller-identity
}

#
#
#

alias keeper='BROWSER=/usr/bin/chromium keeperpasswordmanager'

#
# VPN shenanigans
#
alias -g pp="https_proxy=http://localhost:8118 http_proxy=http://localhost:8118"