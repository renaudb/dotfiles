# Set path.
export PATH="$(brew --prefix)/bin:$PATH"

# Configure ohmyzsh.
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME=""
plugins=(brew git uv)
source $ZSH/oh-my-zsh.sh

# Set prompt.
PROMPT="%{$fg[cyan]%}%~%{$reset_color%}"
PROMPT+=' $(git_prompt_info)'

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[blue]%}git:(%{$fg[red]%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[blue]%}) %{$fg[yellow]%}%1{✗%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%})"

# Set default editor.
export VISUAL="emacs -nw"
export EDITOR="$VISUAL"

# Alias bazel with blaze.
alias blaze=bazel

# Utils
alias jsonpp="python3 -m json.tool"
alias csv2json="python -c 'import sys; import pandas as pd; df = pd.read_csv(sys.stdin); df.to_json(sys.stdout, orient=\"records\")'"

# Source local changes.
source ~/.zshrc.local
