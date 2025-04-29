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
  echo
  echo apcd aws sso login
  apcd aws sso login

  echo
  echo apcp aws sso login
  apcp aws sso login

  echo
  echo apcp aws sts get-caller-identity
  apcp aws sts get-caller-identity

  echo
  echo apcd aws sts get-caller-identity
  apcd aws sts get-caller-identity
}

#
# VPN shenanigans
#
alias -g pp="https_proxy=http://localhost:8118 http_proxy=http://localhost:8118"