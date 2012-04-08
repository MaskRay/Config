#!/bin/zsh
# vim:fdm=marker

# 预配置 {{{
# 如果不是交互shell就直接结束 (unix power tool, 2.11)
if [[  "$-" != *i* ]]; then return 0; fi
source /etc/profile.d/autojump.zsh

# 为兼容旧版本定义 is-at-least 函数
function is-at-least {
    local IFS=".-" min_cnt=0 ver_cnt=0 part min_ver version

    min_ver=(${=1})
    version=(${=2:-$ZSH_VERSION} 0)

    while (( $min_cnt <= ${#min_ver} )); do
      while [[ "$part" != <-> ]]; do
        (( ++ver_cnt > ${#version} )) && return 0
        part=${version[ver_cnt]##*[^0-9]}
      done

      while true; do
        (( ++min_cnt > ${#min_ver} )) && return 0
        [[ ${min_ver[min_cnt]} = <-> ]] && break
      done

      (( part > min_ver[min_cnt] )) && return 0
      (( part < min_ver[min_cnt] )) && return 1
      part=''
    done
}

SHELL=`which zsh`

# 定义颜色 {{{
if [[ ("$TERM" = *256color || "$TERM" = screen*) && -f $HOME/.lscolor256 ]]; then
    #use prefefined colors
    eval $(dircolors -b $HOME/.lscolor256)
    use_256color=1
    export TERMCAP=${TERMCAP/Co\#8/Co\#256}
else
    [[ -f $HOME/.lscolor ]] && eval $(dircolors -b $HOME/.lscolor)
fi

#color defined for prompts and etc
autoload colors
[[ $terminfo[colors] -ge 8 ]] && colors
pR="%{$reset_color%}%u%b" pB="%B" pU="%U"
for i in red green blue yellow magenta cyan white black; {eval pfg_$i="%{$fg[$i]%}" pbg_$i="%{$bg[$i]%}"}
#}}}
#}}}

# 设置参数 {{{
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

#remove / and . from WORDCHARS to allow alt-backspace to delete word
WORDCHARS='*?_-[]~=&;!#$%^(){}<>'

#report to me when people login/logout
watch=(notme)
#replace the default beep with a message
#ZBEEP="\e[?5h\e[?5l"        # visual beep

#is-at-least 4.3.0 &&

# 自动加载自定义函数
fpath=($HOME/.zsh/site-functions/ $fpath)
# 需要设置了extended_glob才能glob到所有的函数，为了补全能用，又需要放在compinit前面
autoload -U ${fpath[1]}/*(:t)
# }}}

# 命令补全参数{{{
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
#zstyle ':completion:*' user-expand _pinyin
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

# }}}

# 自定义函数 {{{

# 普通自定义函数 {{{
#show 256 color tab
256tab() {
    for k in `seq 0 1`;do
        for j in `seq $((16+k*18)) 36 $((196+k*18))`;do
            for i in `seq $j $((j+17))`; do
                printf "\e[01;$1;38;5;%sm%4s" $i $i;
            done;echo;
        done;
    done
}

#alarm using atd
alarm() {
    echo "msg ${argv[2,-1]} && aplay -q ~/.sounds/MACSound/System\ Notifi.wav" | at now + $1 min
}

#calculator
calc()  { awk "BEGIN{ print $* }" ; }

#check if a binary exists in path
bin-exist() {[[ -n ${commands[$1]} ]]}

#recalculate track db gain with mp3gain
(bin-exist mp3gain) && id3gain() { find $* -type f -iregex ".*\(mp3\|ogg\|wma\)" -exec mp3gain -r -s i {} \; }

#ccze for log viewing
(bin-exist ccze) && lless() { tac $* |ccze -A |less }

#man page to pdf
(bin-exist ps2pdf) && man2pdf() {  man -t ${1:?Specify man as arg} | ps2pdf -dCompatibility=1.3 - - > ${1}.pdf; }

# }}}


#{{{-----------------functions to set gnu screen title----------------------
# active command as title in terminals
case $TERM in
    xterm*|rxvt*)
        function title() { print -nP "\e]0;$1\a" }
        ;;
    screen*)
        #only set screen title if it is in a local shell
        if [ -n $STY ] && (screen -ls |grep $STY &>/dev/null); then
            function title()
            {
                #modify screen title
                print -nP "\ek$1\e\\"
                #modify window title bar
                #print -nPR $'\033]0;'$2$'\a'
            }
        elif [ -n $TMUX ]; then       # actually in tmux !
            function title() {  print -nP "\e]2;$1\a" }
        else
            function title() {}
        fi
        ;;
    *)
        function title() {}
        ;;
esac

#set screen title if not connected remotely
#if [ "$STY" != "" ]; then
screen_precmd() {
    #a bell, urgent notification trigger
    #echo -ne '\a'
    #title "`print -Pn "%~" | sed "s:\([~/][^/]*\)/.*/:\1...:"`" "$TERM $PWD"
    title "`print -Pn "%~" |sed "s:\([~/][^/]*\)/.*/:\1...:;s:\([^-]*-[^-]*\)-.*:\1:"`" "$TERM $PWD"
    echo -ne '\033[?17;0;127c'
}

screen_preexec() {
    local -a cmd; cmd=(${(z)1})
    case $cmd[1]:t in
        'ssh')          title "@""`echo $cmd[2]|sed 's:.*@::'`" "$TERM $cmd";;
        'sudo')         title "#"$cmd[2]:t "$TERM $cmd[3,-1]";;
        'for')          title "()"$cmd[7] "$TERM $cmd";;
        'ls'|'ll')      ;;
        *)              title $cmd[1]:t "$TERM $cmd[2,-1]";;
    esac
}

#}}}

#{{{-----------------define magic function arrays--------------------------
if ! (is-at-least 4.3); then
    #the following solution should work on older version <4.3 of zsh.
    #The "function" keyword is essential for it to work with the old zsh.
    #NOTE these function fails dynamic screen title, not sure why
    #CentOS stinks.
    function precmd() {
        screen_precmd
    }

    function preexec() {
        screen_preexec
        pwd_color_preexec
    }

    function chpwd() {
        pwd_color_chpwd
    }
else
    #this works with zsh 4.3.*, will remove the above ones when possible
    typeset -ga preexec_functions precmd_functions chpwd_functions
    precmd_functions+=screen_precmd
    preexec_functions+=screen_preexec
    preexec_functions+=pwd_color_preexec
    chpwd_functions+=pwd_color_chpwd
fi

#}}}

# }}}

# 提示符 {{{
if [ "$SSH_TTY" = "" ]; then
    local host="$pB$pfg_magenta%m$pR"
else
    local host="$pB$pfg_red%m$pR"
fi
local user="$pB%(!:$pfg_red:$pfg_green)%n$pR"       #different color for privileged sessions
local symbol="$pB%(!:$pfg_red# :$pfg_yellow> )$pR"
local job="%1(j,$pfg_red:$pfg_blue%j,)$pR"
#PROMPT='$user$pfg_yellow@$pR$host$job$symbol'
#PROMPT2="$PROMPT$pfg_cyan%_$pR $pB$pfg_black>$pR$pfg_green>$pB$pfg_green>$pR "
#NOTE  **DO NOT** use double quote , it does not work
typeset -A altchar
set -A altchar ${(s..)terminfo[acsc]}
PR_SET_CHARSET="%{$terminfo[enacs]%}"
PR_SHIFT_IN="%{$terminfo[smacs]%}"
PR_SHIFT_OUT="%{$terminfo[rmacs]%}"
#PR_RSEP=$PR_SET_CHARSET$PR_SHIFT_IN${altchar[\`]:-|}$PR_SHIFT_OUT
#RPROMPT='$__PROMPT_PWD'

# SPROMPT - the spelling prompt
SPROMPT="${pfg_yellow}zsh$pR: correct '$pfg_red$pB%R$pR' to '$pfg_green$pB%r$pR' ? ([${pfg_cyan}Y$pR]es/[${pfg_cyan}N$pR]o/[${pfg_cyan}E$pR]dit/[${pfg_cyan}A$pR]bort) "

#行编辑高亮模式 {{{
if (is-at-least 4.3); then
    zle_highlight=(region:bg=magenta
                   special:bold,fg=magenta
                   default:bold
                   isearch:underline
                   )
fi
#}}}

# }}}

# 键盘定义及键绑定 {{{
#bindkey "\M-v" "\`xclip -o\`\M-\C-e\""
# 设置键盘 {{{
# create a zkbd compatible hash;
# to add other keys to this hash, see: man 5 terminfo
autoload -U zkbd
bindkey -e      #use emacs style keybindings :(
typeset -A key  #define an array

#if zkbd definition exists, use defined keys instead
if [[ -f ~/.zkbd/${TERM}-${DISPLAY:-$VENDOR-$OSTYPE} ]]; then
    source ~/.zkbd/$TERM-${DISPLAY:-$VENDOR-$OSTYPE}
else
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
fi

# setup key accordingly
[[ -n "${key[Home]}"    ]]  && bindkey  "${key[Home]}"    beginning-of-line
[[ -n "${key[End]}"     ]]  && bindkey  "${key[End]}"     end-of-line
[[ -n "${key[Insert]}"  ]]  && bindkey  "${key[Insert]}"  overwrite-mode
[[ -n "${key[Delete]}"  ]]  && bindkey  "${key[Delete]}"  delete-char
[[ -n "${key[Up]}"      ]]  && bindkey  "${key[Up]}"      up-line-or-history
[[ -n "${key[Down]}"    ]]  && bindkey  "${key[Down]}"    down-line-or-history
[[ -n "${key[Left]}"    ]]  && bindkey  "${key[Left]}"    backward-char
[[ -n "${key[Right]}"   ]]  && bindkey  "${key[Right]}"   forward-char

# }}}

# 键绑定  {{{
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
#bindkey "" history-beginning-search-backward-end
#bindkey "" history-beginning-search-forward-end
bindkey -M viins "" history-beginning-search-backward-end
bindkey -M viins "" history-beginning-search-forward-end
bindkey '[1;5D' backward-word     # C-left
bindkey '[1;5C' forward-word      # C-right

autoload -U edit-command-line
zle -N      edit-command-line
bindkey '\ee' edit-command-line

autoload -U url-quote-magic
zle -N self-insert url-quote-magic
# }}}

# }}}

# ZLE 自定义widget {{{
#

# {{{ pressing TAB in an empty command makes a cd command with completion list
# from linuxtoy.org
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

# {{{ colorize commands
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

# 拼音补全
function _pinyin() { reply=($($HOME/bin/chsdir 0 $*)) }

# {{{ double ESC to prepend "sudo"
sudo-command-line() {
    [[ -z $BUFFER ]] && zle up-history
    [[ $BUFFER != sudo\ * ]] && BUFFER="sudo $BUFFER"
    zle end-of-line                 #光标移动到行末
}
zle -N sudo-command-line
#定义快捷键为： [Esc] [Esc]
bindkey "\e\e" sudo-command-line
# }}}

# {{{ c-z to continue
bindkey -s "" "fg\n"
# }}}

# {{{ esc-enter to run program in screen split region
function run-command-in-split-screen() {
    screen -X eval \
        "focus bottom" \
        split \
        "focus bottom" \
        "screen $HOME/bin/screen_run $BUFFER" \
        "focus top"
    zle kill-buffer
}
zle -N run-command-in-split-screen
bindkey "\e\r" run-command-in-split-screen
# }}}

# }}}

# 环境变量及其他参数 {{{
# number of lines kept in history
export HISTSIZE=10000
# number of lines saved in the history after logout
export SAVEHIST=10000
# location of history
export HISTFILE=/tmp/.zsh_history_$UID
export MENUCONFIG_COLOR=blackbg

export PATH=$HOME/.cabal/bin:~/.gem/ruby/1.9.1/bin:$HOME/bin:$HOME/bin/ssh:$PATH
export PATH=$PATH:/opt/scala-2.10.0-M2/bin
export PATH=$PATH:/opt/icedtea-bin-7.2.0/bin
export EDITOR=vim
export VISUAL=vim
export SUDO_PROMPT=$'[\e[31;5msudo\e[m] password for \e[33;1m%p\e[m: '
export INPUTRC=$HOME/.inputrc

#MOST like colored man pages
export PAGER=less
# export LESS_TERMCAP_md=$'\E[1;31m'      #bold1
# export LESS_TERMCAP_mb=$'\E[1;31m'
# export LESS_TERMCAP_me=$'\E[m'
# export LESS_TERMCAP_so=$'\E[01;7;34m'  #search highlight
# export LESS_TERMCAP_se=$'\E[m'
# export LESS_TERMCAP_us=$'\E[1;2;32m'    #bold2
# export LESS_TERMCAP_ue=$'\E[m'
export LESS="-M -i -R --shift 5"
export LESSCHARSET=utf-8
export READNULLCMD=less
# In archlinux the pipe script is in PATH, how ever in debian it is not
(bin-exist src-hilite-lesspipe.sh) && export LESSOPEN="| src-hilite-lesspipe.sh %s"
[ -x /usr/share/source-highlight/src-hilite-lesspipe.sh ] && export LESSOPEN="| /usr/share/source-highlight/src-hilite-lesspipe.sh %s"

#for ConTeX
#source $HOME/.context_env /home/roylez/soft/ConTeXt/tex

#for gnuplot, avoid locate!!!
#export GDFONTPATH=$(dirname `locate DejaVuSans.ttf | tail -1`)
[[ -n $DISPLAY ]] && export GDFONTPATH=/usr/share/fonts/TTF

# redefine command not found
(bin-exist cowsay) && (bin-exist fortune) && command_not_found_handler() { fortune -s| cowsay -W 70}

# }}}

# 读入其他配置 {{{

# 主机特定的配置，前置的主要原因是有可能需要提前设置PATH等环境变量
#   例如在aix主机，需要把 /usr/linux/bin
#   置于PATH最前以便下面的配置所调用的命令是linux的版本
[[ -f $HOME/.zshrc.$HOST ]] && source $HOME/.zshrc.$HOST
[[ -f $HOME/.zshrc.local ]] && source $HOME/.zshrc.local
# }}}

# 命令别名 {{{
# alias and listing colors
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....=../../../..
alias pg='pgrep -l'
alias cp='cp -i -v'
alias mv='mv -i -v'
alias rm='rm -i -v'
alias psg='ps aux|grep'
alias clip='xsel -ib <'
if [ `uname` = 'Linux' ]; then
    alias ls=$'ls -XF --color=auto --time-style="+\e[33m[\e[32m%Y-%m-%d \e[35m%k:%M\e[33m]\e[m"'
    alias l='ls -l'
    alias la='l -A'
else
    alias ls="ls -F"
    alias l="ls -l"
    alias la='l -A'
fi
alias -g A="|awk"
alias -g B='|sed -r "s:\x1B\[[0-9;]*[mK]::g"'       # remove color, make things boring
alias -g C="|wc"
alias -g E="|sed"
alias -g G='|GREP_COLOR=$(echo 3$[$(date +%N)%6+1]'\'';1;7'\'') egrep -i --color=always'
alias -g H="|head -n $(($LINES-2))"
alias -g L="|less"
alias -g P="|column -t"
alias -g R="|tac"
alias -g S="|sort"
alias -g T="|tail -n $(($LINES-2))"
alias -g X="|xargs"
alias -g N='> /dev/null'
alias -g NF="./*(oc[1])"      # last modified(inode time) file or directory
alias -g MOU='-o users,uid=1000,gid=1000,codepage=936,utf8'

# tmux or screen ?
(bin-exist tmux) && alias s=tmux || alias s=screen

#file types
(bin-exist apvlv) && alias -s pdf=apvlv
alias -s ps=gv
for i in jpg png;           alias -s $i=gqview
for i in avi rmvb wmv;      alias -s $i=mplayer
for i in rar zip 7z lzma;   alias -s $i="7z x"

#no correct for mkdir mv and cp
#for i in mkdir mv cp;       alias $i="nocorrect $i"
alias find='noglob find'        # noglob for find
alias grep='grep -I --color=auto'
alias egrep='egrep -I --color=auto'
alias cal='cal -3'
alias freeze='kill -STOP'
alias ls=$'ls -h --color=auto -X --time-style="+\e[33m[\e[32m%Y-%m-%d \e[35m%k:%M\e[33m]\e[m"'
alias df='df -Th'
alias du='du -h'
#show directories size
alias dud='du -s *(/)'
#date for US and CN
alias adate='for i in US/Eastern Australia/{Brisbane,Sydney} Asia/{Hong_Kong,Singapore} Europe/Paris; do printf %-22s "$i:";TZ=$i date +"%m-%d %a %H:%M";done'
#bloomberg radio
alias bloomberg='mplayer mms://media2.bloomberg.com/wbbr_sirus.asf'
alias pyprof='python -m cProfile'
alias python='nice python'
alias info='info --vi-keys'
alias rsync='rsync --progress --partial'
alias port='netstat -ntlp'
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias e='emacsclient -c -t'
alias v='vim --servername GVIM --remote-tab-silent'
alias wgetpaste='wgetpaste -C'
alias -s B='|sed -r "s:\x1B\[[0-9;]*[mK]::g"'
alias g2u='iconv -f GBK -t UTF-8'
alias u2g='iconv -f UTF-8 -t GBK'
alias df='df -hT'
alias luit='luit -encoding gbk'
alias c=cat
alias L=less
alias x=xargs
alias dropboxd=/opt/dropbox/dropboxd
alias gdb='gdb -q'
alias getmail='getmail -r rc0 -r rc1'
alias sv='sudo vim'
alias eme='sudo emerge -1'
alias peme='sudo proxychains emerge -1'
alias emel='tail -f /var/log/emerge.log'
alias emef='tail -f /var/log/emerge-fetch.log'
alias ei='eix -uI --only-names'
alias eiu='FORMAT="<installedversions:I>" I="<category>/<name>-<version>[<use>]\n" eix'

alias -g EG='|& egrep'
alias -g EH='|& head'
alias -g ET='|& tail'
alias -g G='| egrep'
alias -g H='| head'
alias -g T='| tail'
alias -g NUL='>&- 2>&-'
alias -g X='| xargs'
alias -g X0='| xargs -0'

# }}}1

# {{{1 path aliases
# cd ~p <=> cd /home/ray/projects
hash -d e='/etc'
hash -d t='/tmp'
hash -d d="/home/ray/Documents"
hash -d a="/home/ray/algo"
hash -d p="/home/ray/projects"
hash -d u='/usr'
hash -d us='/usr/share'
hash -d usd='/usr/share/doc'
#export CDPATH=$HOME:/usr/share:$HOME/Documents:$HOME/algo
# }}}1

# {{{1 suffix aliases
for i in jpg png gif; alias -s $i=display
for i in rar zip 7z lzma; alias -s $i="7z x"
for i in odf doc; alias -s $i=lowriter
for i in c cpp h; alias -s $i=e
for i in pdf ps; alias -s $i=evince
for i in html htm xml; alias -s $i=firefox
# }}}1




RESET='[00m'
RED='[01;31m'
GREEN='[01;32m'
YELLOW='[01;33m'
BLUE='[01;34m'
MAGENTA='[01;35m'
CYAN='[01;36m'
WHITE='[01;37m'
UNDERLINE='[04m'

header()
{
  begin_str=">>>"
  end_str=">>>"
  banner="Gentoo Linux"
  length=$((${COLUMNS} - 1 - ${#begin_str} - ${#end_str} - ${#banner} - 2))
  half=$(($length / 2))
  line=`printf '%*s' $half`
  echo -en "%{\e${RED}%}${begin_str}\e${WHITE}${line// /-}[\e${MAGENTA}${banner}\e${WHITE}]${line// /-}\e${RED}${end_str}\e${RESET}"
}

get_hg_repos_id()
{
  hg id -bint 2> /dev/null
}

get_git_repos_branch()
{
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/\1/"
}

get_git_repos_id()
{
  git log --oneline -1
}

repos_info()
{
  HG_REPOS_ID="$(get_hg_repos_id)"
  if [ "${HG_REPOS_ID}" ]
  then
    echo "\n\e${WHITE}[\e${CYAN}Mercurial\e${WHITE}] Revision ID \e${YELLOW}${HG_REPOS_ID}"
  else
    GIT_REPOS_BRANCH="$(get_git_repos_branch)"
    if [ "${GIT_REPOS_BRANCH}" ]
    then
	GIT_REPOS_ID="$(get_git_repos_id)"
	echo "\n\e${WHITE}[\e${CYAN}Git\e${WHITE}] Current Branch \e${YELLOW}${GIT_REPOS_BRANCH}\e${WHITE} Commit SHA1 \e${YELLOW}${GIT_REPOS_ID}"
    else
      echo
    fi
  fi
}

get_repos_info()
{
    echo "$(repos_info)"
}

my_prompt()
{
    echo -en "\e${BLUE}\u256d\u2500\e${CYAN}\e${GREEN}%n \e${RESET}at \e${WHITE}%m \e${RESET}in \e${BLUE}%d"
    echo '\n\u2570\u2500%(?..[$: %?] )%{\e${RED}%}%(#.#.%%) %{\e${RESET}%}'
}

autoload -U promptinit colors
promptinit
colors

typeset -ga chpwd_functions
PROMPT="$(my_prompt)"
#chpwd_functions+='get_repos_info'

MAIL=/var/spool/mail/ray && export MAIL
export MASTER_SITE_OVERRIDE=ftp://ftp.freebsdchina.org/pub/FreeBSD/ports/distfiles/${DIST_SUBDIR}/

bindkey -s '^zh' "htop\n"
bindkey -s '^zl' "ls\n"
bindkey -s '^zL' "l\n"
bindkey -s '^zp' "import /tmp/screen.jpg\n"
bindkey -s "^zw" "(ip l sh wlan0 | grep -q DOWN; a=\${\${?/0/up}/1/down}; ip l s wlan0 \$a; echo \$a)\n"
bindkey -s '^zP' "sleep 3 && import -window root /tmp/screen.jpg\n"
