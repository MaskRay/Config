#!/bin/zsh
# vim:sw=2 sts=2 et fdm=marker

if [[  "$-" != *i* ]]; then return 0; fi

# Server? {{{1
if (( ! ${+WINDOWID} )) {
  if [[ -d ~/gentoo ]]; then
    export EPREFIX=~/gentoo
    export PATH="$EPREFIX/usr/bin:$EPREFIX/bin:$EPREFIX/tmp/usr/bin:$EPREFIX/tmp/bin:$PATH"
  fi
  alias -g halt=
  alias -g poweroff=
  alias -g shutdown=
  alias -g reboot=
}

# Parameters & environment variables {{{1
WORDCHARS='*?_-[]~=&;!#$%^(){}<>'
export PATH=~/bin:~/.local/bin:~/bin/ssh:$PATH
export EDITOR=vim
export LESS="-MiR --shift 5"
export MENUCONFIG_COLOR=blackbg
export SUDO_PROMPT=$'[\e[31;5msudo\e[m] password for \e[33;1m%p\e[m: '
export PAGER='less -s' # squeeze blank lines
export PYTHONSTARTUP=$HOME/.pythonstartup
#export NVIM_TUI_ENABLE_TRUE_COLOR=1 # neovim true color

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

# Look {{{1
PROMPT=$'%F{blue}\u256d\u2500%F{CYAN}%B%F{cyan}%n %F{white}@ %F{magenta}%m %F{white}>>= %F{green}%~ %1(j,%F{red}:%j,)%b\n%F{blue}\u2570\u2500%B%(?..[%?] )%{%F{red}%}%# %F{white}%b'
#. /usr/share/zsh/site-contrib/zsh-syntax-highlighting.zsh

# dircolors {{{2
if [[ "$TERM" = *256color && -f $HOME/.lscolor256 ]]; then
    eval $(dircolors -b ~/.lscolor256)
else if [[ -f $HOME/.lscolor ]];
    eval $(dircolors -b ~/.lscolor)
fi

# Options {{{1
# History {{{2
HISTSIZE=100000
SAVEHIST=10000
HISTFILE=~/.history/zsh
unsetopt flowcontrol
setopt hist_ignore_all_dups     # when runing a command several times, only store one
setopt hist_reduce_blanks       # reduce whitespace in history
setopt hist_ignore_space        # do not remember commands starting with space
setopt share_history            # share history among sessions
setopt extended_history         # timestamp for each history entry
setopt hist_verify              # reload full command when runing from history
setopt hist_expire_dups_first   # remove dups when max size reached
setopt inc_append_history       # append to history once executed
setopt notify                   # report the status of backgrounds jobs immediately

# Directories {{{2
setopt auto_cd                  # if not a command, try to cd to it.
setopt auto_pushd               # automatically pushd directories on dirstack
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

for i ({1..9}) alias $i="cd +$i"; unset i

setopt no_complete_aliases
setopt auto_continue            #automatically send SIGCON to disowned jobs
setopt extended_glob            # so that patterns like ^() *~() ()# can be used
setopt pushd_ignore_dups        # do not push dups on stack
setopt brace_ccl                # expand alphabetic brace expressions
setopt correct                  # spell check for commands only
setopt equals
setopt no_hist_beep             # don not beep on history expansion errors
setopt hash_list_all            # search all paths before command completion
setopt interactive_comments     # comments in history
setopt list_types               # show ls -F style marks in file completion
setopt long_list_jobs           # show pid in bg job list
setopt numeric_glob_sort        # when globbing numbered files, use real counting
setopt prompt_subst             # prompt more dynamic, allow function in prompt
setopt nonomatch
setopt nobeep

#fpath=($HOME/.zsh/site-functions/ $fpath)
fpath=($HOME/Util/zsh-completions/src/ $fpath)

# Completion {{{1
autoload -U compinit
compinit
setopt AUTO_LIST
setopt AUTO_MENU
setopt MENU_COMPLETE
setopt complete_in_word   # complete /v/c/a/p
setopt no_nomatch		  # enhanced bash wildcard completion
setopt magic_equal_subst
setopt noautoremoveslash
setopt null_glob

# ignore the current directory
zstyle ':completion:*:cd:*' ignore-parents parent pwd

zstyle ':completion:*' use-cache true
#zstyle ':completion::complete:*' cache-path .zcache
zstyle ':completion:*' verbose yes
zstyle ':completion:*' menu select
zstyle ':completion:*:*:default' force-list always
zstyle ':completion:*' select-prompt '%SSelect:  lines: %L  matches: %M  [%p]'
zstyle ':completion:*:match:*' original only
zstyle ':completion::prefix-1:*' completer _complete
zstyle ':completion:predict:*' completer _complete
zstyle ':completion:incremental:*' completer _complete _correct

# Path Completion
zstyle ':completion:*' expand 'yes'
zstyle ':completion:*' squeeze-shlashes 'yes'
zstyle ':completion::complete:*' '\\'

# Colorful Completion
export ZLSCOLORS="${LS_COLORS}"
zmodload zsh/complist
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'

# Fix case and typo
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*:approximate:*' max-errors 1 numeric

# Grouping Completion
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:descriptions' format $'\e[01;33m -- %d --\e[0m'
#zstyle ':completion:*:messages' format $'\e[01;35m -- %d --\e[0m'
#zstyle ':completion:*:warnings' format $'\e[01;31m -- No Matches Found --\e[0m'
#zstyle ':completion:*:corrections' format $'\e[01;32m -- %d (errors: %e) --\e[0m'

# huge list
zstyle ':completion:*:default' list-prompt '%S%M matches%s'
zstyle ':completion:*:default' menu 'select=0'
# Completing order
zstyle ':completion:*:-tilde-:*' group-order 'named-directories' 'path-directories' 'users' 'expand'
zstyle ':completion:*' completer _complete _prefix _user_expand _correct _prefix _match
# newer file first
#zstyle ':completion:*' file-sort modification reverse
# Separate man page sections.
zstyle ':completion:*:manuals' separate-sections true
# complete with a menu for xwindow ids
#zstyle ':completion:*:windows' menu on=0
#zstyle ':completion:*:expand:*' tag-order all-expansions

#kill completion
compdef pkill=kill
compdef pkill=killall
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:*:*:processes' force-list always
zstyle ':completion:*:processes' command 'ps -au$USER '

#zstyle ':completion:*' file-patterns '%p(^-/):globbed-files' '*(-/):directories' '*:all-files'
zstyle ':completion::complete:feh:*' file-patterns '*.png *.jpg *.bmp *.gif *(/):directories'
zstyle ':completion::complete:wps:*' file-patterns '*.doc *.docx *(/):directories'
zstyle ':completion::complete:wpp:*' file-patterns '*.ppt *.pptx *(/):directories'
zstyle ':completion::complete:llpp:*' file-patterns '*.pdf *(/):directories'
zstyle ':completion::complete:et:*' file-patterns '*.xls *.xlsx *(/):directories'

# vim ignore
zstyle ':completion:*:*:vim:*:*files' ignored-patterns '*.(avi|mkv|rmvb|pyc|wmv)'

zstyle ':completion::*:(-command-|export):*' fake-parameters CFLAGS CXXFLAGS LD_LIBRARY_PATH

# Don't complete uninteresting users...
zstyle ':completion:*:*:*:users' ignored-patterns \
  adm amanda apache avahi beaglidx bin cacti canna chrony clamav colord daemon dbus distcache dnsmasq dovecot etcd fax fcron ftp games gdm git gkrellmd gopher hacluster haldaemon halt hsqldb http ident junkbust ldap lp mail mailman mailnull memcached mldonkey mongodb mysql nagios named nbd netdump news nfsnobody nobody nscd ntp nut nx openvpn operator pcap polipo polkitd postfix postgres privoxy proxy pulse pvm quagga radvd redis rpc rpcuser rpm rtkit shutdown squid sshd sync systemd-bus-proxy systemd-journal-gateway systemd-journal-remote systemd-journal-upload systemd-network systemd-resolve systemd-timesync tor unbound usbmux uucp uuidd vcsa xfs

zstyle ':completion:*:mutt:*' users ${${${(f)"$(<~/.mutt/aliases)"}#alias[[:space:]]}%%[[:space:]]*}

# ... completion
user_complete(){
	if [[ -z $BUFFER ]]; then
		return
	fi
	if [[ $BUFFER =~ "^\.\.\.*$" ]]; then
		BUFFER=`echo "$BUFFER" |sed 's/^/cd\ /g'`
		zle end-of-line
		user_complete
		return
	elif [[ $BUFFER =~ ".*\.\.\..*$" ]] ;then
		BUFFER=`echo "$BUFFER" |sed 's/\.\.\./\.\.\/\.\./g'`
		zle end-of-line
		user_complete
		return
	fi
	zle expand-or-complete
	#recolor-cmd
}
zle -N user_complete
autoload compinstall

delete-horizontal-space() {
  if [[ $LBUFFER == *\  ]]; then
    LBUFFER="${LBUFFER%% *} "
  fi
}
zle -N delete-horizontal-space

bindkey -M menuselect '^o' accept-and-infer-next-history

#check if a binary exists in path
bin-exist() {[[ -n ${commands[$1]} ]]}

# Bindings {{{1
bindkey -e
bindkey "\t" user_complete
bindkey '\e\\' delete-horizontal-space
bindkey '^xh' _complete_help
#bindkey '^p' history-beginning-search-backward
#bindkey '^n' history-beginning-search-forward

# Aliases & functions {{{1
# General aliases & functions (partially shared with bash) {{{2
. ~/.alias

# terminfo {{{1
bindkey  "${terminfo[khome]}"    beginning-of-line
bindkey  "${terminfo[kend]}"     end-of-line
bindkey  "${terminfo[kich1]}"    overwrite-mode
bindkey  "${terminfo[kbs]}"      backward-delete-char # original: kbs=^H (\177, Debian)
bindkey  "${terminfo[kcuu1]}"    up-line-or-history
bindkey  "${terminfo[kcud1]}"    down-line-or-history
bindkey  "${terminfo[kcub1]}"    backward-char
bindkey  "${terminfo[kcuf1]}"    forward-char
bindkey  "${terminfo[kdch1]}"    delete-char # original: kdch1=\E[3~

# ZLE {{{1
# prepend-sudo {{{2
prepend-sudo() {
  [[ $BUFFER != su(do|)\ * ]] && { BUFFER="sudo $BUFFER"; ((CURSOR += 5)); }
}
zle -N prepend-sudo
# highlight {{{2
zle_highlight=(region:bg=magenta
  special:bold,fg=magenta
  default:bold
  isearch:underline
  suffix:fg=cyan
)

# edit-command-line {{{2
autoload -U edit-command-line
zle -N      edit-command-line
bindkey '\C-x\C-e' edit-command-line

bindkey "\eq" push-line-or-edit
bindkey '^x^f' vi-find-next-char
bindkey '^xf' vi-find-next-char
bindkey '^xb' vi-find-prev-char
bindkey '^x^s' prepend-sudo

# url-quote-magic {{{2
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# fasd {{{2
eval "$(fasd --init posix-alias zsh-hook zsh-wcomp zsh-wcomp-install)"
bindkey '^X^A' fasd-complete    # C-x C-a to do fasd-complete (fils and directories)
bindkey '^X^F' fasd-complete-f  # C-x C-f to do fasd-complete-f (only files)
bindkey '^X^D' fasd-complete-d  # C-x C-d to do fasd-complete-d (only directories)
if (( ${+WINDOWID} )) {
  alias v='fasd -fie "vim --servername GVIM --remote-tab-silent"'
} else {
  alias v='fasd -fie vim'
}
alias j='fasd_cd -d'
alias jj='fasd_cd -d -i'

# Goodies {{{1
(bin-exist cowsay) && (bin-exist fortune) && command_not_found_handler() { fortune -s| cowsay -W 70}

# Imports {{{1

# OPAM
[[ -s ~/.opam/opam-init/init.zsh ]] && . ~/.opam/opam-init/init.zsh

# archlinuxcn/pinyin-completion
[[ -s /usr/share/pinyin-completion/shell/pinyin-comp.zsh ]] && . /usr/share/pinyin-completion/shell/pinyin-comp.zsh

# aur/fzf
if [[ -s /etc/profile.d/fzf.zsh ]] then
  source /etc/profile.d/fzf.zsh
  # redefine __fzfcmd (appending `-e` option) to disable fuzzy matching
  __fzfcmd() {
    [ ${FZF_TMUX:-1} -eq 1 ] && echo "fzf-tmux -e -d${FZF_TMUX_HEIGHT:-40%}" || echo "fzf -e"
  }
fi

# rvm (Ruby)
[[ -s ~/.rvm/scripts/rvm ]] && . ~/.rvm/scripts/rvm

# nvm (Node.js)
[[ -s ~/.nvm/nvm.sh ]] && . ~/.nvm/nvm.sh

# Environment Modules {{{1
module() { eval `~/bin/modulecmd.tcl zsh $*`; }
module use ~/.modules
module load ghc go nim perl ruby/2.2.0 texlive/2015 wps #mpi/impi
