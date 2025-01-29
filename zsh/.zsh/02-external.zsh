##
## z - jump around
##

PATH_Z="/usr/share/z/z.sh"
PATH_STARSHIP="/usr/bin/starship"
PATH_SYNTAX_HIGHLIGHTING="/usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
PATH_FZF_KEY="/usr/share/fzf/key-bindings.zsh"
PATH_FZF_COMP="/usr/share/fzf/completion.zsh"
PATH_ZSH_SITE_FUNCTIONS="/usr/share/zsh/site-functions"

if [[ "$OS" == "mac" ]]; then
  PATH_Z="/opt/homebrew/etc/profile.d/z.sh"
  PATH_STARSHIP="/opt/homebrew/bin/starship"
  PATH_SYNTAX_HIGHLIGHTING="/opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
  PATH_FZF_KEY="/opt/homebrew/opt/fzf/shell/completion.zsh"
  PATH_FZF_COMP="/opt/homebrew/opt/fzf/shell/key-bindings.zsh"

   if type brew &>/dev/null; then
      PATH_ZSH_SITE_FUNCTIONS=$(brew --prefix)/share/zsh-completions
    fi
fi

[[ -r "$PATH_Z" ]] && source "$PATH_Z"

# vendored
if [[ -r "$HOME/.zsh/lib/fz.sh" ]] ; then
  source $HOME/.zsh/lib/fz.sh
fi

##
## starship - prompt
##
[[ -r "$PATH_STARSHIP" ]] && eval "$(starship init zsh)"

##
## zsh-syntax-highlighting
##

if [[ -r "$PATH_SYNTAX_HIGHLIGHTING" ]] ; then
  source "$PATH_SYNTAX_HIGHLIGHTING"

  ##
## color overrides
##
  ZSH_HIGHLIGHT_STYLES[default]='none'
  ZSH_HIGHLIGHT_STYLES[unknown-token]='fg=red'
  ZSH_HIGHLIGHT_STYLES[reserved-word]='fg=yellow'
  ZSH_HIGHLIGHT_STYLES[alias]='fg=blue'
  ZSH_HIGHLIGHT_STYLES[builtin]='fg=blue'
  ZSH_HIGHLIGHT_STYLES[function]='fg=blue'
  ZSH_HIGHLIGHT_STYLES[command]='fg=blue'
  ZSH_HIGHLIGHT_STYLES[precommand]='none'
  ZSH_HIGHLIGHT_STYLES[commandseparator]='none'
  ZSH_HIGHLIGHT_STYLES[hashed-command]='fg=blue'
  ZSH_HIGHLIGHT_STYLES[path]='none'
  ZSH_HIGHLIGHT_STYLES[path_prefix]='none'
  ZSH_HIGHLIGHT_STYLES[path_approx]='fg=yellow'
  ZSH_HIGHLIGHT_STYLES[globbing]='fg=green'
  ZSH_HIGHLIGHT_STYLES[history-expansion]='fg=green'
  ZSH_HIGHLIGHT_STYLES[single-hyphen-option]='fg=magenta'
  ZSH_HIGHLIGHT_STYLES[double-hyphen-option]='fg=red'
  ZSH_HIGHLIGHT_STYLES[back-quoted-argument]='none'
  ZSH_HIGHLIGHT_STYLES[single-quoted-argument]='fg=yellow'
  ZSH_HIGHLIGHT_STYLES[double-quoted-argument]='fg=yellow'
  ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]='fg=cyan'
  ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]='fg=cyan'
  ZSH_HIGHLIGHT_STYLES[assign]='none'
fi

##
## fzf
##

if [[ -r "$PATH_FZF_KEY" ]] ; then
  source "$PATH_FZF_KEY"
  source "$PATH_FZF_COMP"

  # uses `fd` for easy .gitignore integration (faster too!)
  export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
fi


##
## zsh-completions
##

if [[ -d "$PATH_ZSH_SITE_FUNCTIONS" ]] ; then

  FPATH="$PATH_ZSH_SITE_FUNCTIONS:$FPATH"

  autoload -Uz compinit
  compinit -C -d ~/.zcompdump

  # Compile zcompdump in background
  {
    zcompile ~/.zcompdump
  } &!

  #autoload -U /usr/share/zsh/site-functions/*(:t)
  zstyle ':completion:*' auto-description 'specify: %d'
  zstyle ':completion:*' completer _expand _complete _correct _approximate
  zstyle ':completion:*' format 'Completing %d'
  zstyle ':completion:*' group-name ''
#  zstyle ':completion:*' menu select=2 eval "$(dircolors -b)"
  zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
  zstyle ':completion:*' list-colors ''
  zstyle ':completion:*' list-prompt %SAt %p: hit TAB for more, or the character to insert%s
  zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
  zstyle ':completion:*' menu select=long
  zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
  zstyle ':completion:*' use-compctl false
  zstyle ':completion:*' verbose true
  zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
  zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

fi

##
## aws cli completion
##
if [[ -r "/usr/bin/aws_completer" ]] ; then
  autoload bashcompinit && bashcompinit
  complete -C '/usr/bin/aws_completer' aws
fi

