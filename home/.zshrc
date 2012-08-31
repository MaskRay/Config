#!/bin/zsh
# vim:fdm=marker

# 预配置 {{{
# 如果不是交互shell就直接结束 (unix power tool, 2.11)
if [[  "$-" != *i* ]]; then return 0; fi

# server?
if [[ $(hostname) != t* ]]; then
  EPREFIX=
  MYSELF=true
else
  export EPREFIX=~/gentoo
  export PATH="$EPREFIX/usr/bin:$EPREFIX/bin:$EPREFIX/tmp/usr/bin:$EPREFIX/tmp/bin:$PATH"
  export LD_LIBRARY_PATH="$EPREFIX/usr/lib:$EPREFIX/lib"
  alias cabali="cabal install --extra-include-dirs=$EPREFIX/usr/include --extra-lib-dirs=$EPREFIX/lib --extra-lib-dirs=$EPREFIX/usr/lib"
fi
if [[ "$TERM" = *256color && -f $HOME/.lscolor256 ]]; then
    eval $(dircolors -b ~/.lscolor256)
else if [[ -f $HOME/.lscolor ]];
    eval $(dircolors -b ~/.lscolor)
fi

setopt pushdminus
setopt complete_aliases         #do not expand aliases _before_ completion has finished
setopt auto_cd                  # if not a command, try to cd to it.
setopt auto_pushd               # automatically pushd directories on dirstack
setopt auto_continue            #automatically send SIGCON to disowned jobs
setopt extended_glob            # so that patterns like ^() *~() ()# can be used
setopt pushd_ignore_dups        # do not push dups on stack
setopt brace_ccl                # expand alphabetic brace expressions
#setopt chase_links             # ~/ln -> /; cd ln; pwd -> /
setopt complete_in_word         # stays where it is and completion is done from both ends
setopt correct                  # spell check for commands only
#setopt equals extended_glob    # use extra globbing operators
setopt no_hist_beep             # don not beep on history expansion errors
setopt hash_list_all            # search all paths before command completion
setopt hist_ignore_all_dups     # when runing a command several times, only store one
setopt hist_reduce_blanks       # reduce whitespace in history
setopt hist_ignore_space        # do not remember commands starting with space
setopt share_history            # share history among sessions
setopt hist_verify              # reload full command when runing from history
setopt hist_expire_dups_first   #remove dups when max size reached
setopt interactive_comments     # comments in history
setopt list_types               # show ls -F style marks in file completion
setopt long_list_jobs           # show pid in bg job list
setopt numeric_glob_sort        # when globbing numbered files, use real counting
setopt inc_append_history       # append to history once executed
setopt prompt_subst             # prompt more dynamic, allow function in prompt
setopt nonomatch


fpath=($HOME/.zsh/site-functions/ $fpath)
autoload -U ${fpath[1]}/*(:t)

#   zsytle ':completion:*:completer:context or command:argument:tag'
zmodload -i zsh/complist        # for menu-list completion
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}" "ma=${${use_256color+1;7;38;5;143}:-1;7;33}"
#ignore list in completion
zstyle ':completion:*' ignore-parents parent pwd directory
#menu selection in completion
zstyle ':completion:*' menu select=2
#zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*' completer _oldlist _expand _force_rehash _complete _match #_user_expand
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
zstyle ':completion::complete:*' use-cache on
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

#force rehash when command not found
#  http://zshwiki.org/home/examples/compsys/general
_force_rehash() {
    (( CURRENT == 1 )) && rehash
    return 1    # Because we did not really complete anything
}

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

#check if a binary exists in path
bin-exist() {[[ -n ${commands[$1]} ]]}

#recalculate track db gain with mp3gain
(bin-exist mp3gain) && id3gain() { find $* -type f -iregex ".*\(mp3\|ogg\|wma\)" -exec mp3gain -r -s i {} \; }

#ccze for log viewing
(bin-exist ccze) && lless() { tac $* |ccze -A |less }

autoload -U zkbd
bindkey -e      # emacs style
typeset -A key
key[Home]=${terminfo[khome]}
key[End]=${terminfo[kend]}
key[Insert]=${terminfo[kich1]}
key[Delete]=${terminfo[kdch1]}
key[Up]=${terminfo[kcuu1]}
key[Down]=${terminfo[kcud1]}
key[Left]=${terminfo[kcub1]}
key[Right]=${terminfo[kcuf1]}
key[PageUp]=${terminfo[kpp]}
key[PageDown]=${terminfo[knp]}
for k in ${(k)key} ; do
        # $terminfo[] entries are weird in ncurses application mode...
  [[ ${key[$k]} == $'\eO'* ]] && key[$k]=${key[$k]/O/[}
done

# setup key accordingly
[[ -n "${key[Home]}"    ]]  && bindkey  "${key[Home]}"    beginning-of-line
[[ -n "${key[End]}"     ]]  && bindkey  "${key[End]}"     end-of-line
[[ -n "${key[Insert]}"  ]]  && bindkey  "${key[Insert]}"  overwrite-mode
[[ -n "${key[Delete]}"  ]]  && bindkey  "${key[Delete]}"  delete-char
[[ -n "${key[Up]}"      ]]  && bindkey  "${key[Up]}"      up-line-or-history
[[ -n "${key[Down]}"    ]]  && bindkey  "${key[Down]}"    down-line-or-history
[[ -n "${key[Left]}"    ]]  && bindkey  "${key[Left]}"    backward-char
[[ -n "${key[Right]}"   ]]  && bindkey  "${key[Right]}"   forward-char

autoload -U edit-command-line
zle -N      edit-command-line
bindkey '\ee' edit-command-line

# pressing TAB in an empty command makes a cd command with completion list
dumb-cd(){
    if [[ -n $BUFFER ]] ; then # 如果该行有内容
        zle expand-or-complete # 执行 TAB 原来的功能
    else # 如果没有
        BUFFER="cd " # 填入 cd（空格）
        zle end-of-line # 这时光标在行首，移动到行末
        zle expand-or-complete # 执行 TAB 原来的功能
    fi
}
zle -N dumb-cd
bindkey "\t" dumb-cd #将上面的功能绑定到 TAB 键

# colorize commands
TOKENS_FOLLOWED_BY_COMMANDS=('|' '||' ';' '&' '&&' 'sudo' 'do' 'time' 'strace')

recolor-cmd() {
    region_highlight=()
    colorize=true
    start_pos=0
    for arg in ${(z)BUFFER}; do
        ((start_pos+=${#BUFFER[$start_pos+1,-1]}-${#${BUFFER[$start_pos+1,-1]## #}}))
        ((end_pos=$start_pos+${#arg}))
        if $colorize; then
            colorize=false
            res=$(LC_ALL=C builtin type $arg 2>/dev/null)
            case $res in
                *'reserved word'*)   style="fg=magenta,bold";;
                *'alias for'*)       style="fg=cyan,bold";;
                *'shell builtin'*)   style="fg=yellow,bold";;
                *'shell function'*)  style='fg=green,bold';;
                *"$arg is"*)
                    [[ $arg = 'sudo' ]] && style="fg=red,bold" || style="fg=blue,bold";;
                *)                   style='none,bold';;
            esac
            region_highlight+=("$start_pos $end_pos $style")
        fi
        [[ ${${TOKENS_FOLLOWED_BY_COMMANDS[(r)${arg//|/\|}]}:+yes} = 'yes' ]] && colorize=true
        start_pos=$end_pos
    done
}

check-cmd-self-insert() { zle .self-insert && recolor-cmd }
check-cmd-backward-delete-char() { zle .backward-delete-char && recolor-cmd }

zle -N self-insert check-cmd-self-insert
zle -N backward-delete-char check-cmd-backward-delete-char

# double ESC to prepend "sudo"
sudo-command-line() {
    [[ -z $BUFFER ]] && zle up-history
    [[ $BUFFER != sudo\ * ]] && BUFFER="sudo $BUFFER"
    zle end-of-line                 #光标移动到行末
}
zle -N sudo-command-line
#定义快捷键为： [Esc] [Esc]
bindkey "\e\e" sudo-command-line


PROMPT=$'%F{blue}\u256d\u2500%F{CYAN}%B%F{cyan}%n %F{white}@ %B%F{magenta}%m %F{white}>>= %B%F{green}%~ %1(j,%F{red}:%j,)\n%F{blue}\u2570\u2500%(?..[$: %?] )%{%F{red}%}%# %{${RESET}%}'
WORDCHARS='*?_-[]~=&;!#$%^(){}<>'
HISTSIZE=10000
SAVEHIST=10000
HISTFILE=~/.zsh_history

export MENUCONFIG_COLOR=blackbg
export SUDO_PROMPT=$'[\e[31;5msudo\e[m] password for \e[33;1m%p\e[m: '

export PATH=$HOME/.cabal/bin:~/.gem/ruby/1.9.1/bin:$HOME/bin:$HOME/bin/ssh:$PATH
PATH=$PATH:~/.npm/coffee-script/1.3.3/package/bin
unset RUBYOPT

export LESS="-M -i -R --shift 5"
export LESSCHARSET=utf-8

# redefine command not found
(bin-exist cowsay) && (bin-exist fortune) && command_not_found_handler() { fortune -s| cowsay -W 70}

eval "$(fasd --init posix-alias zsh-hook zsh-ccomp zsh-ccomp-install       zsh-wcomp zsh-wcomp-install)"
bindkey '^X^A' fasd-complete    # C-x C-a to do fasd-complete (fils and directories)
bindkey '^X^F' fasd-complete-f  # C-x C-f to do fasd-complete-f (only files)
bindkey '^X^D' fasd-complete-d  # C-x C-d to do fasd-complete-d (only directories)
alias o='fasd -fe xdg-open'
alias sv='fasd -fe "sudo vim"'
alias e='fasd -fe "emacsclient -c -n"'
alias j='fasd_cd -d'
alias m='fasd -fe mplayer'
if [[ -n $MYSELF ]]; then
  alias v='fasd -fe "vim --servername GVIM --remote-tab-silent"'
else
  alias v='fasd -fe vim'
fi

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....=../../../..
alias pg='pgrep -l'
alias cp='cp -i -v'
alias mv='mv -i -v'
alias rm='rm -i -v'
alias psg='ps aux|grep'
alias clip='xsel -ib'

if [ `uname` = 'Linux' ]; then
    alias ls=$'ls -XF --color=auto --time-style="+\e[33m[\e[32m%Y-%m-%d \e[35m%k:%M\e[33m]\e[m"'
else
    alias ls="ls -F"
fi
alias l="ls -l"
alias la='l -A'

alias -g A="|awk"
alias -g B='|sed -r "s:\x1B\[[0-9;]*[mK]::g"'       # remove color, make things boring
alias -g C="|wc"
alias -g E="|sed"
alias -g G='|GREP_COLOR=$(echo 3$[$(date +%N)%6+1]'\'';1;7'\'') egrep -i --color=always'
alias -g H="|head -n $(($LINES-2))"
alias -g L="|less"
alias -g P="|column -t"
alias -g S="|sort"
alias -g T="|tail -n $(($LINES-2))"
alias -g X="|xargs"
alias -g NF="./*(oc[1])"      # last modified(inode time) file or directory
alias -g MOU='-o users,uid=1000,gid=1000,codepage=936,utf8'
alias win='WINEPATH="d:/mingw/bin;d:/mingw/msys/1.0/bin" wine'
alias fh='firefox ~/haskell/index.html'

alias c=cat
alias L=less
alias g='grep -I --color=auto'
alias eg='egrep -I --color=auto'
alias df='df -Th'
alias du='du -h'
alias dud='du -s *(/)' #show directories size
alias adate='for i in US/Eastern Australia/{Brisbane,Sydney} Asia/{Hong_Kong,Singapore} Europe/Paris; do printf %-22s "$i:";TZ=$i date +"%m-%d %a %H:%M";done' #date for US and CN
alias rsync='rsync --progress --partial'
alias port='netstat -ntlp'
if [[ -n $MYSELF ]]; then
  alias eme='sudo emerge -1 --keep-going'
  alias deme='sudo FEATURE=distcc emerge -1 --keep-going'
else
  alias v='fasd -fe vim'
  alias eme='emerge -1 --keep-going'
fi
alias wgetpaste='wgetpaste -C'
alias -s B='|sed -r "s:\x1B\[[0-9;]*[mK]::g"'
alias 2pdf='libreoffice --headless --convert-to pdf'
alias 2csv='libreoffice --headless --convert-to csv'
alias g2u='iconv -f GBK -t UTF-8'
alias u2g='iconv -f UTF-8 -t GBK'
alias ntp='sudo /etc/init.d/ntp-client start'
alias luit='luit -encoding gbk'
alias dropboxd=/opt/dropbox/dropboxd
alias gdb='gdb -q'
alias getmail='getmail -r rc0 -r rc1'

# gentoo specific
alias peme='sudo proxychains emerge -1'
alias emel='tail -f /var/log/emerge.log'
alias emef='tail -f /var/log/emerge-fetch.log'
alias ei='eix -uI --only-names'
alias eiu='FORMAT="<installedversions:I>" I="<category>/<name>-<version>[<use>]\n" eix'
alias disp='sudo dispatch-conf'

# global aliases
alias -g EG='|& egrep'
alias -g EH='|& head'
alias -g ET='|& tail'
alias -g G='| egrep'
alias -g H='| head'
alias -g T='| tail'
alias -g N='> /dev/null'
alias -g NN='>&- 2>&-'
alias -g X='| xargs'
alias -g X0='| xargs -0'

# suffix aliases
for i in jpg png gif; alias -s $i=feh
for i in avi rmvb wmv; alias -s $i=mplayer
for i in xlsx xls ppt pptx docx doc; alias -s $i=libreoffice
for i in pdf ps; alias -s $i=evince
for i in html htm xml; alias -s $i=firefox

zle_highlight=(region:bg=magenta
  special:bold,fg=magenta
  default:bold
  isearch:underline
)

bindkey -s "" "fg\n"
bindkey -s "^zw" "(ip l sh wlan0 | grep -q DOWN; a=\${\${?/0/up}/1/down}; ip l s wlan0 \$a; echo \$a)\n"
bindkey -s '^zP' "sleep 3 && import -window root /tmp/screen.jpg\n"
cowfiles=(/usr/share/cowsay-3.03/cows/*)
bindkey -s '^zm' "toilet -f bigmono12 --gay<<<'hi all';sleep 2\n"'while :; do fortune -s | cowsay -f${cowfiles[$RANDOM % ${#cowfiles[@]} + 1]}; sleep 0.3; done'"\n"
