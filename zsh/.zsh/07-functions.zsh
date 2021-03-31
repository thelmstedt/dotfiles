
##
## go_temp - make a temp dir and cd
##
function go_temp() { cd $(mktemp -d) }
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
	  local current="$(pwd)"
	  local matched_dir=""
	  local running=true

	  if [[ $mode == "GIT" ]] ; then
      while [ "$running" = true ]; do
        IGNORE_HOME=$(echo "$current" | sed "s|$HOME||") # we never want to go back to home
        if [[ -e "$current/.git" ]] ; then
          running=false
          matched_dir="$current"
        elif [[ "$current" == "$HOME" ]] ; then # stop at home
          running=false
        else
          matched_dir="$current"
          current="$(dirname ${current})"
        fi
      done
    elif [[ $mode == "FOLDER" ]]; then
      # NOTE, remove ^ from pattern to match anywhere..
      local awk_script="{a=0; for (i = NF-1; i > 0; i--) { if (\$i ~ /^$target/) { a=1; }; if (a == 1) { print \$i } }}"
      # reversed fields until it starts with $target | reverse back to normal | join with "/" to make a path
      matched_dir=$(pwd | awk -F'/' $awk_script  | tac | tr '\n' '/')
    fi

	  if [ -n "$matched_dir" ]; then
		    cd ${matched_dir}
	  else
		    echo "No Match." >&2
		    return 1
	  fi
}
alias u=upto

##
## awk_column - for ease of use in pipes e.g. `ps aux | grep foo | ac 2 | xargs kill -9`
##
function awk_column() {
  local cmd="{print \$$1}"
  if [ -z "$2" ]
  then
    awk ${cmd}
  else
    awk -F"$2" ${cmd}
  fi

}
alias ac=awk_column

function fzf-git-branch() {
  git branch --sort=-committerdate |
    cut -c 3- |
    fzf --multi --preview="git log {}" |
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
