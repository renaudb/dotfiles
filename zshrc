# Set prompt.
PROMPT='%F{green}%n@%m%f:%F{cyan}%~%f%# '

# Set options
setopt NO_BEEP
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY

# Set ls output to colourized.
alias ls='ls -G'

# Set cp, mv and rm to prompt on overwrite.
alias cp="cp -i"
alias mv="mv -i"
alias rm="rm -I"

# Source local changes.
source ~/.zshrc.local
