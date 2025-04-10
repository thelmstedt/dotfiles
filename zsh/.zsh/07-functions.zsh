
##
## go_temp - make a temp dir and cd
##
function go_temp() {
  cd "$(mktemp -d)" || (echo "cd to mktemp failed?" && exit)
}
alias gt=go_temp



##
## upto - navigate up to a directory by prefix, or to the .git root
##
function upto() {
    local target="$1"
    local mode="FOLDER"
	  if [ -z "$target" ]; then
		    mode="GIT"
	  fi
	  local matched_dir=""

	  if [[ $mode == "GIT" ]] ; then
      cd $(git rev-parse --show-toplevel)
      return 1
    elif [[ $mode == "FOLDER" ]]; then
      # NOTE, remove ^ from pattern to match anywhere..
      local awk_script="{a=0; for (i = NF-1; i > 0; i--) { if (\$i ~ /^$target/) { a=1; }; if (a == 1) { print \$i } }}"
      # reversed fields until it starts with $target | reverse back to normal | join with "/" to make a path
      matched_dir=$(pwd | awk -F'/' $awk_script  | tac | tr '\n' '/')
      if [ -n "$matched_dir" ]; then
          cd "${matched_dir}" || (echo "cd to ${matched_dir} failed?" && exit)
      else
          echo "No Match." >&2
          return 1
      fi
    fi

}
alias u=upto

##
## awk_column - provide 1-or-many integers to select columns
## -F arg will pass to awk as normal field-separator
##
## good for ease of use in pipes e.g. `ps aux | grep foo | ac 2 | xargs kill -9`
##
## ac -F~ 3 2 1 -> awk -F~ {print $3" "$2" "$1}
##
##
function awk_column() {
  if [ -z "$1" ]
  then
    echo "Requires at least 1 positional arg eg: 'ac N' for Nth arg"
    return 1
  fi
  zparseopts -D -E -- F+:=separator
  separator=("${(@)separator:#-F}")

  cmd="\$$1" #required arg
  if [ ! -z "$2" ]
  then
    cmd="$cmd$(printf '" "$%s' "${@:2}")" #optionally additional args separated by space
  fi
  # shellcheck disable=SC2128
  if [ -n "$separator" ]
  then
    awk -F "$separator" "{print $cmd}" #-F separator
  else
    awk "{print $cmd}"
  fi
}
alias ac=awk_column

function fzf-git-branch() {
  git branch --sort=-committerdate |
    cut -c 3- |
    fzf --multi --preview="git log {} --" |
    xargs --no-run-if-empty git checkout
}
alias gb=fzf-git-branch

function fnano() {
  F="$(fzf)"
  if [ -z "$F" ]
  then
      #nothing
      echo "Cancelled"
    else
      nano $F
  fi
}

function fless() {
  F="$(fzf)"
  if [ -z "$F" ]
  then
      #nothing
      echo "Cancelled"
    else
      less $F
  fi
}

function vv() {
  echo "Creating new virtualenv at ./.venv"
  deactivate
  uv venv --allow-existing .venv
  echo "Activating venv"
  source ./.venv/bin/activate
  [ -f requirements.txt ] && echo "Installing deps from requirements.txt" && uv pip install -r requirements.txt
  [ -f requirements_tmv.txt ] && echo "Installing deps from requirements_tmv.txt" && uv pip install -r requirements_tmv.txt
  [ -f pyproject.toml ] && echo "Installing deps from pyproject.toml" && uv pip install -r pyproject.toml
}

function __tigr() {
  if [ -z "$1" ]
  then
    if [ $(git rev-parse --verify master 2>/dev/null) ]
    then
      branch='master'
    elif [ $(git rev-parse --verify main 2>/dev/null) ]
    then
      branch='main'
    fi
  else
    branch=$1
  fi
  echo tig $branch.. --reverse
  tig $branch.. --reverse
}

alias tigr=__tigr

