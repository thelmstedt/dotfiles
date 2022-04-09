# Python
[[ -s "$HOME/.pythonrc.py" ]] && export PYTHONSTARTUP="$HOME/.pythonrc.py"
[ -f /opt/miniconda3/etc/profile.d/conda.sh ] && source /opt/miniconda3/etc/profile.d/conda.sh
export PYTHONIOENCODING=UTF-8

alias w="cd $HOME/work"
alias b="cd $HOME/work/tmv"

function kt() {
  echo kubectl exec --stdin --tty -n default "$1" -- tail -f "$2"
  kubectl exec --stdin --tty -n default "$1" -- tail -f "$2"
}

function ktmv () {
  kt "$1" /tmv/ceeqtm/logs/django/exceptions.log
}