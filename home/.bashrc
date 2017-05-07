shopt -s checkwinsize cmdhist extglob globstar histappend

PS1='\e[1;36m\u \e[m@ \e[1;36mhacking \e[m>>= \e[1;32m\w\n\[\e[1;31m\]\$ \[\e[m\]'
PS4='(${BASH_SOURCE}:${LINENO}): ${FUNCNAME[0]} - [${SHLVL},${BASH_SUBSHELL}, $?]'

prepend_path() {
  [[ ":$PATH:" != *"$1"* ]] && PATH="$1:$PATH"
}

HISTCONTROL=ignoreboth
HISTSIZE=10000
HISTTIMEFORMAT='%F %T '

alias rm='rm -i'
alias mv='mv -i'
alias ll='ls -l'

#bind '\C-w:unix-filename-rubout'
#bind '\C-t:unix-word-rubout'

[[ -f /etc/profile.d/bash-completion.sh ]] && source /etc/profile.d/bash-completion.sh

. ~/.alias

prepend_path $HOME/.local/bin
prepend_path $HOME/.rvm/bin
#export PATH="$HOME/.local/bin:$HOME/.rvm/bin:$PATH" # Add RVM to PATH for scripting
