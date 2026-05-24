# Python
[[ -s "$HOME/.pythonrc.py" ]] && export PYTHONSTARTUP="$HOME/.pythonrc.py"
[ -f /opt/miniconda3/etc/profile.d/conda.sh ] && source /opt/miniconda3/etc/profile.d/conda.sh
export PYTHONIOENCODING=UTF-8

alias w="cd $HOME/work"
alias b="cd $HOME/work/tmv"

#
# AWS
#


awsenv() {
  local profile="$1"
  shift
  (
    # some things only work with profile defined
    export AWS_PROFILE="$profile"
    # this expands the profile to env vars - MOST tools will pick these up preferentially and avoid any subsequent sso
    # this is important for terraform because each separate module SSOs :eyeroll:
    source <(aws configure export-credentials --profile "$profile" --format env)
    "$@"
  )
}


alias -g apatms='awsenv atms'
alias -g apwa='awsenv wa-dev'

alias -g apcd='awsenv clarivate'
alias -g apcp='awsenv clarivate-prod'


function aws_login () {
  set -x
  apcd BROWSER=/usr/bin/chromium aws sso login
  apcp BROWSER=/usr/bin/chromium aws sso login
  apcp BROWSER=/usr/bin/chromium aws sts get-caller-identity
  apcd BROWSER=/usr/bin/chromium aws sts get-caller-identity
}

#
# VPN shenanigans
#

# enter into vpn netns
vpndo() {
  VPN_CONTEXT=1 command nsenter --target $(docker inspect --format "{{.State.Pid}}" vpn-vpn-1) --net --setuid $(id -u) "$@"
}

# vpn proxy
alias -g pp="https_proxy=http://localhost:8118 http_proxy=http://localhost:8118"

# wrapper for platform terraform.sh scrips to do the right thing
tf() {
  local module="$1"
  local cmd="$2"
  shift 2

  local profile=""
  if [[ -z "$AWS_PROFILE" ]]; then
    case "$module" in
      prod*) profile="clarivate-prod" ;;
      *) profile="clarivate" ;;
    esac
    echo "detected [$profile] from [$module] directory"
  fi

  if [[ -n "$profile" ]]; then
    echo "using profile: $profile"
    awsenv "$profile" ./terraform.sh "$module" "$cmd" "$@"
  else
    echo "using profile: $AWS_PROFILE"
    ./terraform.sh "$module" "$cmd" "$@"
  fi
}

clvssh () {
  SSH_ASKPASS=~/.ssh-askpass.sh SSH_ASKPASS_REQUIRE=force vpndo ssh $1
}
