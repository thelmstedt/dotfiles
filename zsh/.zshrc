
##
## load config
##
for config (~/.zsh/*.zsh) source $config
export PATH="/opt/homebrew/opt/postgresql@15/bin:$PATH"

# bun completions
[ -s "/Users/tim/.bun/_bun" ] && source "/Users/tim/.bun/_bun"

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
