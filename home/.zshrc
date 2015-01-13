#!/bin/zsh
# vim:sw=2 sts=2 et fdm=marker

if [[  "$-" != *i* ]]; then return 0; fi

# Server? {{{1
if [[ $(hostname) = lap ]]; then
  MYSELF=true
else
  if [[ -d ~/gentoo ]]; then
    export EPREFIX=~/gentoo
    export PATH="$EPREFIX/usr/bin:$EPREFIX/bin:$EPREFIX/tmp/usr/bin:$EPREFIX/tmp/bin:$PATH"
  fi
  alias -g halt=
  alias -g poweroff=
  alias -g shutdown=
  alias -g reboot=
fi

# Parameters & environment variables {{{1
WORDCHARS='*?_-[]~=&;!#$%^(){}<>'
#export SCALA_HOME=/opt/scala-2.10.1
#export PATH=$SCALA_HOME/bin:/opt/texlive/2012/bin/x86_64-linux:$HOME/.cabal/bin:$HOME/bin:~/.local/bin:~/.gem/ruby/2.1.0/bin:$HOME/bin/ssh:$PATH:/home/ray/Cross/toolchain-mips_r2_gcc-4.3.3+cs_uClibc-0.9.30.1/usr/bin
export PATH=$HOME/bin:~/.local/bin:~/.cabal/bin:$HOME/bin/ssh:$PATH
export EDITOR=vim
#export PATH=$PATH:/home/ray/.local/opt/admb-11-linux-gcc4.6.1-64bit/bin
export LESS="-MiR --shift 5"
export MENUCONFIG_COLOR=blackbg
export SUDO_PROMPT=$'[\e[31;5msudo\e[m] password for \e[33;1m%p\e[m: '
#export WINEPATH=z:\\opt\\mingw\\i686-w64-mingw32\\lib
export PAGER='less -s' # squeeze blank lines
export PYTHONSTARTUP=$HOME/.pythonstartup
export GOPATH=~/go

export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
#export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

# Look {{{1
PROMPT=$'%F{blue}\u256d\u2500%F{CYAN}%B%F{cyan}%n %F{white}@ %B%F{magenta}%m %F{white}>>= %B%F{green}%~ %1(j,%F{red}:%j,)\n%F{blue}\u2570\u2500%(?..[%?] )%{%F{red}%}%# %F{white}'
#. /usr/share/zsh/site-contrib/zsh-syntax-highlighting.zsh

# dircolors {{{2
if [[ "$TERM" = *256color && -f $HOME/.lscolor256 ]]; then
    eval $(dircolors -b ~/.lscolor256)
else if [[ -f $HOME/.lscolor ]];
    eval $(dircolors -b ~/.lscolor)
fi

# fasd {{{1
eval "$(fasd --init posix-alias zsh-hook zsh-wcomp zsh-wcomp-install)"
bindkey '^X^A' fasd-complete    # C-x C-a to do fasd-complete (fils and directories)
bindkey '^X^F' fasd-complete-f  # C-x C-f to do fasd-complete-f (only files)
bindkey '^X^D' fasd-complete-d  # C-x C-d to do fasd-complete-d (only directories)
if [[ -n $MYSELF ]]; then
  alias v='fasd -fe "vim --servername GVIM --remote-tab-silent"'
else
  alias v='fasd -fe vim'
fi
alias j='fasd_cd -d'
alias jj='fasd_cd -d -i'
alias o='f -fe xdg-open'

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

alias 1='cd -'
alias 2='cd +2'
alias 3='cd +3'
alias 4='cd +4'
alias 5='cd +5'
alias 6='cd +6'
alias 7='cd +7'
alias 8='cd +8'
alias 9='cd +9'

setopt complete_aliases         #do not expand aliases _before_ completion has finished
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
zstyle ':completion::complete:*' cache-path .zcache
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

# vim ignore
zstyle ':completion:*:*:vim:*:*files' ignored-patterns '*.(avi|mkv|rmvb|pyc|wmv)'

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
bindkey -v '^A' vi-insert-bol
bindkey -v '^E' vi-insert-eol
bindkey -v '^K' kill-line
bindkey -v '\ef' forward-word
bindkey -v '\eb' backward-word
bindkey -v '^P' up-history
bindkey -v '^N' down-history
bindkey -v '^F' forward-char
bindkey -v '^B' backward-char
bindkey -v '^U' kill-whole-line
bindkey -v '^R' history-incremental-search-backward
bindkey -v '^S' history-incremental-search-forward
bindkey -v '^Y' yank-pop
bindkey -v '\e.' insert-last-word
bindkey -v '\e?' which-command
bindkey -v '\eh' run-help
bindkey -v '\el' down-case-word
bindkey -v '\eu' up-case-word
bindkey -v "^[m" copy-prev-shell-word
bindkey '^]' vi-find-next-char

bindkey -e
bindkey -N mymap emacs
bindkey "\t" user_complete
bindkey '\e\\' delete-horizontal-space
#bindkey '^p' history-beginning-search-backward
#bindkey '^n' history-beginning-search-forward

# Aliases {{{1
# General {{{2

# Global aliases {{{2
alias -g E="|sed"
alias -g L="|less"
alias -g P="|column -t"
alias -g S="|sort"
alias -g X="|xargs"
alias -g G='|egrep --color=auto'
alias -g EG='|& egrep --color=auto'
alias -g H="|head -n $(($LINES-2))"
alias -g T="|tail -n $(($LINES-2))"
alias -g N='>/dev/null'
alias -g NN='>/dev/null 2>&1'
alias -g X='| xargs'
alias -g X0='| xargs -0'

# Suffix aliases {{{2
alias -s B='|sed -r "s:\x1B\[[0-9;]*[mK]::g"'

# Path aliases
hash -d up=/usr/portage
hash -d ep=/etc/portage
hash -d vl=/var/lib
hash -d vl=/var/lib
hash -d as=~/Assignment/2014spr
hash -d d=~/Documents

# Application-specific {{{2
. ~/.alias

# ZLE {{{1
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

# url-quote-magic
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

# Goodies {{{1
(bin-exist cowsay) && (bin-exist fortune) && command_not_found_handler() { fortune -s| cowsay -W 70}

cl() {
  cd $1 && ls -a
}

ssht() {
  ssh -t $1 'tmux a || tmux'
}

# listing stuff
#a2# Execute \kbd{ls -lSrah}
alias dir="command ls -lSrah"
#a2# Only show dot-directories
alias lad='command ls -d .*(/)'
#a2# Only show dot-files
alias lsa='command ls -a .*(.)'
#a2# Only files with setgid/setuid/sticky flag
alias lss='command ls -l *(s,S,t)'
#a2# Only show symlinks
alias lsl='command ls -l *(@)'
#a2# Display only executables
alias lsx='command ls -l *(*)'
#a2# Display world-{readable,writable,executable} files
alias lsw='command ls -ld *(R,W,X.^ND/)'
#a2# Display the ten biggest files
alias lsbig="command ls -flh *(.OL[1,10])"
#a2# Only show directories
alias lsd='command ls -d *(/)'
#a2# Only show empty directories
alias lse='command ls -d *(/^F)'
#a2# Display the ten newest files
alias lsnew="command ls -rtlh *(D.om[1,10])"
#a2# Display the ten oldest files
alias lsold="command ls -rtlh *(D.Om[1,10])"
#a2# Display the ten smallest files
alias lssmall="command ls -Srl *(.oL[1,10])"
#a2# Display the ten newest directories and ten newest .directories
alias lsnewdir="command ls -rthdl *(/om[1,10]) .*(D/om[1,10])"
#a2# Display the ten oldest directories and ten oldest .directories
alias lsolddir="command ls -rthdl *(/Om[1,10]) .*(D/Om[1,10])"

#f5# List files which have been modified within the last {\it n} days, {\it n} defaults to 1
modified() {
  print -l -- *(m-${1:-1})
}

# Utils {{{1
# show 256 color tab
256tab() {
    for k in `seq 0 1`;do
        for j in `seq $((16+k*18)) 36 $((196+k*18))`;do
            for i in `seq $j $((j+17))`; do
                printf "\e[01;$1;38;5;%sm%4s" $i $i;
            done;echo;
        done;
    done
}

cowfiles=(/usr/share/cowsay-3.03/cows/*)
bindkey -s '^zm' "toilet -f bigmono12 --gay<<<'hi all';sleep 2\n"'while :; do fortune -s | cowsay -f${cowfiles[$RANDOM % ${#cowfiles[@]} + 1]}; sleep 0.3; done'"\n"

# OPAM configuration
. $HOME/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# Pinyin Completion
if [[ -d $HOME/.zsh/Pinyin-Completion ]]; then
  PATH=$PATH:$HOME/.zsh/Pinyin-Completion/bin
  source $HOME/.zsh/Pinyin-Completion/shell/pinyin-comp.zsh
fi

# Environment Modules {{{1
module() { eval `tclsh ~/bin/modulecmd.tcl zsh $*`; }
module use ~/.modules
module load ruby ghc perl texlive/2014 wps #mpi/impi

# rvm
[[ -s ~/.rvm/scripts/rvm ]] && . ~/.rvm/scripts/rvm

# nvm
[[ -s ~/.nvm/nvm.sh ]] && . ~/.nvm/nvm.sh

bindkey  "${terminfo[khome]}"    beginning-of-line
bindkey  "${terminfo[kend]}"     end-of-line
bindkey  "${terminfo[kich1]}"    overwrite-mode
bindkey  "${terminfo[kbs]}"      backward-delete-char # original: kbs=^H (\177, Debian)
bindkey  "${terminfo[kcuu1]}"    up-line-or-history
bindkey  "${terminfo[kcud1]}"    down-line-or-history
bindkey  "${terminfo[kcub1]}"    backward-char
bindkey  "${terminfo[kcuf1]}"    forward-char
bindkey  "${terminfo[kdch1]}"    delete-char # original: kdch1=\E[3~
