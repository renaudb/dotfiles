# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install

# The following lines were added by compinstall
zstyle :compinstall filename '$HOME/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# Set prompt.
autoload -U colors && colors
PROMPT="[%{$fg[magenta]%}%n%{$reset_color%}:%{$fg[blue]%}%~%{$reset_color%}] "

# Load dir colors.
eval `dircolors $HOME/.dir_colors`

# Set ls to colour output.
alias ls="/bin/ls --color"

# Set ll to detailed list.
alias ll="ls -l"

# Set cp, mv and rm to prompt on overwrite.
alias cp="cp -i"
alias mv="mv -i"
alias rm="rm -I"

# Set emacs aliases.
alias e="emacsclient -t"
alias ec="emacsclient -c"
alias vim="emacsclient -t"
alias vi="emacsclient -t"

# Set feh alias.
alias feh="feh -Z --force-aliasing"

# Add paths to PATH.
PATH="$HOME/bin:/usr/local/MATLAB/R2011a/bin:$PATH"
