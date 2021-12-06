# Set prompt.
PROMPT='%F{green}%n@%m%f:%F{cyan}%~%f%# '

# Set options
setopt NO_BEEP

# Set ls output to colourized.
alias ls='ls -G'

# Set cp, mv and rm to prompt on overwrite.
alias cp="cp -i"
alias mv="mv -i"
alias rm="rm -I"

# Source local changes.
source ~/.zshrc.local
