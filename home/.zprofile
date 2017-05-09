export EDITOR=vim
export LESS="-FiMRwX --shift 5 -z-4"
export MENUCONFIG_COLOR=blackbg
export SUDO_PROMPT=$'[\e[31;5msudo\e[m] password for \e[33;1m%p\e[m: '
export PAGER='less -s' # squeeze blank lines
export PYTHONSTARTUP=$HOME/.pythonstartup
export NVIM_TUI_ENABLE_TRUE_COLOR=1 # neovim true color
export QT_QPA_PLATFORMTHEME=qt5ct

#export LESS_TERMCAP_mb=$'\E[01;31m'
# Start bold mode
if [[ $TERM =~ 256 ]] {
  export LESS_TERMCAP_md=$'\e[1;38;5;178m'
} else {
  export LESS_TERMCAP_md=$'\e[1;31m'
}
# End all mode
export LESS_TERMCAP_me=$'\e[0m'
# Start standout mode e.g. prompt, matches
if [[ $TERM =~ 256 ]] {
  export LESS_TERMCAP_so=$'\e[1;38;5;81m'
} else {
  export LESS_TERMCAP_so=$'\e[1;36m'
}
# End standout mode
export LESS_TERMCAP_se=$'\e[0m'
# Start underlining
if [[ $TERM =~ 256 ]] {
  export LESS_TERMCAP_us=$'\e[4;1;38;5;71m'
} else {
  export LESS_TERMCAP_us=$'\e[4;1;32m'
}
# End underlining
export LESS_TERMCAP_ue=$'\e[0m'

if (( ! ${+SSH_AUTH_SOCK} )) {
  export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/ssh-agent.socket
}
