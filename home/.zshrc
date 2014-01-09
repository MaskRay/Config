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
export SCALA_HOME=/opt/scala-2.10.1
export PATH=$SCALA_HOME/bin:/opt/texlive/2012/bin/x86_64-linux:$HOME/.cabal/bin:$HOME/bin:~/.local/bin:~/.gem/ruby/2.0.0/bin:$HOME/bin/ssh:$PATH:/home/ray/Cross/toolchain-mips_r2_gcc-4.3.3+cs_uClibc-0.9.30.1/usr/bin
export PATH=$SCALA_HOME/bin:~/.cabal/bin:~/bin:~/.local/bin:~/.gem/ruby/2.0.0/bin:~/bin/ssh:$PATH
export EDITOR=vim
#export PATH=$PATH:/home/ray/.local/opt/admb-11-linux-gcc4.6.1-64bit/bin
export LESS="-MiR --shift 5"
export GREP_OPTIONS='--color=auto'
export MENUCONFIG_COLOR=blackbg
export SUDO_PROMPT=$'[\e[31;5msudo\e[m] password for \e[33;1m%p\e[m: '
export WINEPATH=z:\\opt\\mingw\\i686-w64-mingw32\\lib

# Look {{{1
PROMPT=$'%F{blue}\u256d\u2500%F{CYAN}%B%F{cyan}%n %F{white}@ %B%F{magenta}%m %F{white}>>= %B%F{green}%~ %1(j,%F{red}:%j,)\n%F{blue}\u2570\u2500%(?..[$: %?] )%{%F{red}%}%# %F{white}'
#. /usr/share/zsh/site-contrib/zsh-syntax-highlighting.zsh

# dircolors {{{2
if [[ "$TERM" = *256color && -f $HOME/.lscolor256 ]]; then
    eval $(dircolors -b ~/.lscolor256)
else if [[ -f $HOME/.lscolor ]];
    eval $(dircolors -b ~/.lscolor)
fi

# fasd {{{1
eval "$(fasd --init posix-alias zsh-hook zsh-ccomp zsh-ccomp-install       zsh-wcomp zsh-wcomp-install)"
bindkey '^X^A' fasd-complete    # C-x C-a to do fasd-complete (fils and directories)
bindkey '^X^F' fasd-complete-f  # C-x C-f to do fasd-complete-f (only files)
bindkey '^X^D' fasd-complete-d  # C-x C-d to do fasd-complete-d (only directories)
if [[ -n $MYSELF ]]; then
  alias v='fasd -fe "vim --servername GVIM --remote-tab-silent"'
else
  alias v='fasd -fe vim'
fi

# Options {{{1
# History {{{2
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.history/zsh
unsetopt flowcontrol
setopt hist_ignore_all_dups     # when runing a command several times, only store one
setopt hist_reduce_blanks       # reduce whitespace in history
setopt hist_ignore_space        # do not remember commands starting with space
setopt share_history            # share history among sessions
setopt hist_verify              # reload full command when runing from history
setopt hist_expire_dups_first   # remove dups when max size reached
setopt inc_append_history       # append to history once executed

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

fpath=($HOME/.zsh/site-functions/ $fpath)

# Completion {{{1
setopt complete_in_word
setopt always_to_end
#   zsytle ':completion:*:completer:context or command:argument:tag'
zmodload -i zsh/complist        # for menu-list completion
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}" "ma=${${use_256color+1;7;38;5;143}:-1;7;33}"
#ignore list in completion
zstyle ':completion:*' ignore-parents parent pwd directory
#menu selection in completion
zstyle ':completion:*' menu select=2
#zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*' completer _oldlist _expand _complete _match #_user_expand
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric
## case-insensitive (uppercase from lowercase) completion
zstyle ':completion:*' matcher-list 'm:{[:lower:]}={[:upper:]}'
### case-insensitive (all) completion
#zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:*:*:processes' force-list always
zstyle ':completion:*:processes' command 'ps -au$USER'
zstyle ':completion:*:*:kill:*:processes' list-colors "=(#b) #([0-9]#)*=36=1;31"
#use cache to speed up pacman completion
zstyle ':completion::complete:*' use-cache 1
#zstyle ':completion::complete:*' cache-path .zcache
#group matches and descriptions
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:descriptions' format $'\e[33m == \e[1;7;36m %d \e[m\e[33m ==\e[m'
zstyle ':completion:*:messages' format $'\e[33m == \e[1;7;36m %d \e[m\e[0;33m ==\e[m'
zstyle ':completion:*:warnings' format $'\e[33m == \e[1;7;31m No Matches Found \e[m\e[0;33m ==\e[m'
zstyle ':completion:*:corrections' format $'\e[33m == \e[1;7;37m %d (errors: %e) \e[m\e[0;33m ==\e[m'
# dabbrev for zsh!! M-/ M-,
zstyle ':completion:*:history-words' stop yes
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' list false
zstyle ':completion:*:history-words' menu yes select

#autoload -U compinit
autoload -Uz compinit
compinit

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

# Aliases {{{1
# General {{{2

# Global aliases {{{2
alias -g E="|sed"
alias -g L="|less"
alias -g P="|column -t"
alias -g S="|sort"
alias -g X="|xargs"
alias -g G='|egrep'
alias -g EG='|& egrep'
alias -g H="|head -n $(($LINES-2))"
alias -g T="|tail -n $(($LINES-2))"
alias -g N='>/dev/null'
alias -g NN='>/dev/null 2>&1'
alias -g X='| xargs'
alias -g X0='| xargs -0'

# Suffix aliases {{{2
alias -s B='|sed -r "s:\x1B\[[0-9;]*[mK]::g"'

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
. /home/ray/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# Pinyin Completion
[[ -d $HOME/.zsh/Pinyin-Completion ]] && source $HOME/.zsh/Pinyin-Completion/shell/pinyin-comp.zsh && export PATH=$PATH:$HOME/.zsh/Pinyin-Completion/bin

export PERL_LOCAL_LIB_ROOT="$PERL_LOCAL_LIB_ROOT:/home/ray/perl5";
export PERL_MB_OPT="--install_base /home/ray/perl5";
export PERL_MM_OPT="INSTALL_BASE=/home/ray/perl5";
export PERL5LIB="/home/ray/perl5/lib/perl5:$PERL5LIB";
export PATH="/home/ray/perl5/bin:$PATH";
