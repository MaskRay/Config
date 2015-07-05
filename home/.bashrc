shopt -s checkwinsize extglob globstar

PS1='\[\e[1;36m\u \e[m@ \e[1;35mhacking \e[m>>= \e[1;32m\w\n\e[1;31m\]\$ \[\e[m\]' # bug: line-wrap will overlap
PS4='(${BASH_SOURCE}:${LINENO}): ${FUNCNAME[0]} - [${SHLVL},${BASH_SUBSHELL}, $?]'

alias rm='rm -i'
alias mv='mv -i'
alias ll='ls -l'

bind '\C-w:unix-filename-rubout'
bind '\C-t:unix-word-rubout'

[[ -f /etc/profile.d/bash-completion.sh ]] && source /etc/profile.d/bash-completion.sh

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

export NVM_DIR="/home/ray/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
