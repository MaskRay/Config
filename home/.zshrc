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
typeset -U path
path=(~/bin ~/.local/bin "$path[@]")

[[ $TERM = xterm-termite ]] && export TERM=xterm-256color
[[ $TERM = xterm ]] && export TERM=xterm-256color

# Look {{{1
PROMPT=$'%F{blue}%F{CYAN}%B%F{cyan}%n %F{white}@ %F{magenta}%m %F{white}>>= %F{green}%~ %1(j,%F{red}:%j,)%b\n%F{blue}%B%(?..[%?] )%{%F{red}%}%# %F{white}%b'
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
setopt histfcntllock            # use F_SETLCKW
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

fpath=($HOME/Util/zsh-completions/src/ ~/.zsh/functions $fpath)

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
zstyle ':completion:*' cache-path $HOME/.cache/zsh
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
zstyle ':completion:*:processes-names' command  'ps c -u ${USER} -o command | uniq'

#zstyle ':completion:*' file-patterns '%p(^-/):globbed-files' '*(-/):directories' '*:all-files'
zstyle ':completion::complete:display:*' file-patterns '*.{gif,png,jpeg,jpg,svg,webp}:images *(-/):directories'
zstyle ':completion::complete:et:*' file-patterns '*.{xls,xlsx}:files:files *(/):directories'
zstyle ':completion::complete:evince:*' file-patterns '*.{pdf,ps,eps,dvi,djvu,pdf.gz,ps.gz,dvi.gz}:documents:documents *(-/):directories:directories'
zstyle ':completion::complete:feh:*' file-patterns '*.{gif,png,jpeg,jpg,svg}:images:images *(-/):directories:directories'
zstyle ':completion::complete:geeqie:*' file-patterns '*.{gif,png,jpeg,jpg,svg}:images:images *(-/):directories:directories'
zstyle ':completion::complete:llpp:*' file-patterns '*.pdf:files:files *(-/):directories:directories'
zstyle ':completion::complete:wpp:*' file-patterns '*.{ppt,pptx}:files:files *(/):directories'
zstyle ':completion::complete:wps:*' file-patterns '*.{doc,docx}:files:files *(/):directories'
zstyle ':completion::complete:x:*' file-patterns '*.{7z,bz2,gz,rar,tar,tbz,tgz,zip,chm,xz,exe,xpi,apk,maff,crx}:compressed-files:compressed\ files *(-/):directories:directories'

# vim ignore
zstyle ':completion:*:*:vim:*:*files' ignored-patterns '*.(avi|mkv|rmvb|pyc|wmv)'

zstyle ':completion::*:(-command-|export):*' fake-parameters CFLAGS CXXFLAGS LD_LIBRARY_PATH

# Don't complete uninteresting users...
zstyle ':completion:*:*:*:users' ignored-patterns \
  adm amanda apache avahi beaglidx bin cacti canna chrony clamav colord daemon dbus distcache dnsmasq dovecot etcd fax fcron ftp games gdm git gkrellmd gopher hacluster haldaemon halt hsqldb http ident junkbust ldap lp mail mailman mailnull memcached mldonkey mongodb mysql nagios named nbd netdump news nfsnobody nobody nscd ntp nut nx openvpn operator pcap polipo polkitd postfix postgres privoxy proxy pulse pvm quagga radvd redis rpc rpcuser rpm rtkit shutdown squid sshd sync systemd-bus-proxy systemd-journal-gateway systemd-journal-remote systemd-journal-upload systemd-network systemd-resolve systemd-timesync tor unbound usbmux uucp uuidd vcsa xfs
[[ -f ~/.mutt/aliases ]] && zstyle ':completion:*:mutt:*' users ${${${(f)"$(<~/.mutt/aliases)"}#alias[[:space:]]}%%[[:space:]]*}

compdef pgrep=killall
compdef pkill=killall
compdef proxychains=command

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
[[ -f ~/.alias ]] && source ~/.alias

for i in gb gf gh gr gt; do
  zle -N $i
done
bindkey '\C-g\C-b' gb
bindkey '\C-g\C-f' gf
bindkey '\C-g\C-h' gh
bindkey '\C-g\C-r' gr
bindkey '\C-g\C-t' gt

function s() {
  re=$1
  find ${2:-.} -regextype posix-extended -iregex ".*$re.*"
}

# notify-send if the window loses focus or not on the same tmux pane
function n() {
  local window_id=$(xdotool getactivewindow)
  local -a t
  t=($(tmux display-message -p '#D #S #P'))
  local pane_id=$t[1] session_name=$t[2] pane_index=$t[3]
  "$@"
  local retcode=$? cur_window_id=$(xdotool getactivewindow) cur_pane_id=$(tmux display-message -p '#D')
  if [[ $window_id != $cur_window_id || $pane_id != $cur_pane_id ]]; then
    if [[ $retcode != 0 ]]; then
      notify-send -u critical -a "$session_name:$pane_index \$?=$retcode" "$*"
    else
      notify-send -a "$session_name:$pane_index" "$*"
    fi
  fi
  return retcode
}

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

# http://lilydjwg.is-programmer.com/2014/2/2/systemd-user-daemons.42631.html
function juser () {
  # sadly, this won't have nice completion
  typeset -a args
  integer nextIsService=0
  for i; do
    if [[ $i == -u ]]; then
      nextIsService=1
      args=($args _SYSTEMD_CGROUP=/user.slice/user-$UID.slice/user@$UID.service/)
    else
      if [[ $nextIsService -eq 1 ]]; then
        nextIsService=0
        args[$#args]="${args[$#args]}$i.service"
      else
        args=($args $i)
      fi
    fi
  done
  journalctl -n --user ${^args}
}

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

# jump-target {{{2
# https://github.com/scfrazer/zsh-jump-target
if [[ -n $TMUX ]]; then # prevent :T command in neovim from triggering
  autoload -Uz jump-target
  zle -N jump-target
  bindkey "^J" jump-target
fi
# move by shell word {{{2
zsh-word-movement () {
  # see select-word-style for more
  local -a word_functions
  local f

  word_functions=(backward-kill-word backward-word
    capitalize-word down-case-word
    forward-word kill-word
    transpose-words up-case-word)

  if ! zle -l $word_functions[1]; then
    for f in $word_functions; do
      autoload -Uz $f-match
      zle -N zsh-$f $f-match
    done
  fi
  # set the style to shell
  zstyle ':zle:zsh-*' word-style shell
}
zsh-word-movement
unfunction zsh-word-movement
bindkey '^[^b' zsh-backward-word
bindkey '^[^f' zsh-forward-word
bindkey '^[^w' zsh-backward-kill-word
bindkey '^u' backward-kill-line  # was kill-whole-line

   # Imports {{{1

   # archlinuxcn/pinyin-completion
#[[ -s /usr/share/pinyin-completion/shell/pinyin-comp.zsh ]] && . /usr/share/pinyin-completion/shell/pinyin-comp.zsh

# community/fzf
if [[ -s /usr/share/fzf/completion.zsh ]] then
  source /usr/share/fzf/completion.zsh
  source /usr/share/fzf/key-bindings.zsh
  # redefine __fzfcmd (appending `-e` option) to disable fuzzy matching
  #__fzfcmd() {
  #  [ ${FZF_TMUX:-1} -eq 1 ] && echo "fzf-tmux -e -d${FZF_TMUX_HEIGHT:-40%}" || echo "fzf -e"
  #}
fi

# https://github.com/junegunn/dotfiles/blob/master/bashrc
export FZF_CTRL_T_OPTS="--select-1 --exit-0 --preview '(highlight -O ansi -l {} 2> /dev/null || cat {} || tree -C {}) 2> /dev/null | head -200'"
export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window down:3:hidden:wrap --bind '?:toggle-preview' --bind 'ctrl-y:execute-silent(echo -n {2..} | xclip -i)+abort' --header '?:preview C-y:primary'"
if [[ -n ${commands[bfs]} ]]; then
  # /usr/share/fzf/key-bindings.zsh find -> bfs
  export FZF_ALT_C_COMMAND="command bfs -L . -mindepth 1 \\( -path '*/\\.*' -o -fstype 'sysfs' -o -fstype 'devfs' -o -fstype 'devtmpfs' -o -fstype 'proc' \\) -prune -o -type d -print 2> /dev/null | cut -b3-"
  export FZF_CTRL_T_COMMAND="command bfs -L . -mindepth 1 \\( -path '*/\\.*' -o -fstype 'sysfs' -o -fstype 'devfs' -o -fstype 'devtmpfs' -o -fstype 'proc' \\) -prune -o -type f -print -o -type d -print -type l -print 2> /dev/null | cut -b3-"
  export FZF_DEFAULT_COMMAND=$FZF_CTRL_T_COMMAND
fi

if [[ -s /etc/profile.d/autojump.zsh ]]; then
  source /etc/profile.d/autojump.zsh
elif [[ -s /usr/share/autojump/autojump.zsh ]]; then
  source /usr/share/autojump/autojump.zsh
fi

# OCaml
[[ -s ~/.opam/opam-init/init.zsh ]] && source ~/.opam/opam-init/init.zsh

[[ -s ~/.zshrc.local ]] && source ~/.zshrc.local


# command-not-found {{{1
# Load command-not-found on Debian-based distributions.
if [[ -s '/etc/zsh_command_not_found' ]]; then
  source '/etc/zsh_command_not_found'
# Load command-not-found on Arch Linux-based distributions.
elif [[ -s '/usr/share/doc/pkgfile/command-not-found.zsh' ]]; then
  source '/usr/share/doc/pkgfile/command-not-found.zsh'
fi

(($+VTE_VERSION)) && source /etc/profile.d/vte.sh

# Environment Modules {{{1
if [[ -f ~/bin/modulecmd.tcl ]]; then
  module() { eval `~/bin/modulecmd.tcl zsh $*`; }
  module use ~/.modules
  module load go nodejs ruby/2.4.0 rust #nim wps mpi/impi
fi
