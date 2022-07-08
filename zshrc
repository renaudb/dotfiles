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

# Alias bazel with blaze.
alias blaze=bazel

# Utils
alias jsonpp="python3 -m json.tool"
alias csv2json="python -c 'import sys; import pandas as pd; df = pd.read_csv(sys.stdin); df.to_json(sys.stdout, orient=\"records\")'"

# Set default editor.
export VISUAL="emacs -nw"
export EDITOR="$VISUAL"

# Source local changes.
source ~/.zshrc.local
