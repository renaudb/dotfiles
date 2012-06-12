# Check for an interactive session
[ -z "$PS1" ] && return

# -----------------------------------------------------------
# SETUP PROMPT
# -----------------------------------------------------------

#PS1='[\u@\h \W]\$ '
PS1='[\[\e[0;35m\]\u\[\e[m\] \e[0;34m\]\w\[\e[m\]] '

# -----------------------------------------------------------
# AUTOCOMPLETE
# -----------------------------------------------------------

complete -cf sudo
complete -cf man

# -----------------------------------------------------------
# ALIASES AND FUNCTIONS
# -----------------------------------------------------------

# Color ls output
alias ls='ls --color=auto'

# Check if on the Interwebs
alias p8='ping -n 8.8.8.8 -c 4'

# Search files for a word
function srch() {
    find "$1" -type f | xargs grep "$2";
}

# -----------------------------------------------------------
# ENVIRONMENT VARIABLES
# -----------------------------------------------------------

# Set default editor
export EDITOR='emacs'

