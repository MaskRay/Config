function prepend_to_path -d "Prepend the given dir to PATH if it exists and is not already in it"
    if test -d $argv[1]
        if not contains $argv[1] $PATH
            set -gx PATH "$argv[1]" $PATH
        end
    end
end
set PATH ~/bin ~/.local/bin ~/.gem/ruby/1.9.1/bin $PATH

# Functions {{{1
function match
  echo $$argv[2] | grep -q $argv[1]
end

function export
  set -x $argv
end

# Environment variables {{{1
set -x LESS '-MRi --shift 5'
set -x GREP_OPTIONS '--color=auto'
set -x MENUCONFIG_COLOR blackbg

# Set LS_COLORS {{{2
if match 256color TERM; and [ -f ~/.lscolor256 ]
  eval (dircolors -c ~/.lscolor256 | sed 's/env/ -x/')
else if [ -f ~/.lscolor ]
  eval (dircolors -c ~/.lscolor | sed 's/env/ -x/')
end

# Theme {{{1
set fish_color_command yellow
set fish_color_param white
set fish_color_error red -bold

# Greeting {{{2
set fish_greeting ''

# Prompt {{{2
function fish_prompt
  set red \e'[31m'
  set blue \e'[1m'\e'[34m'
  set magenta \e'[1m'\e'[35m'
  set cyan \e'[1m'\e'[36m'
  set white \e'[37m'

  if not set -q __fish_prompt_hostname
    set -g __fish_prompt_hostname (hostname -s)
  end

  switch $USER
    case root
      set user_prompt '#'
    case '*'
      set user_prompt '%'
  end

  echo $blue\u256d\u2500$cyan(whoami) $white@ $magenta$__fish_prompt_hostname $white'>>=' $cyan(prompt_pwd)
  echo -n $blue\u2570\u2500$red$user_prompt(set_color normal)' '
end

# Bindings {{{1

function fish_user_key_bindings
  bind \e. history-token-search-backward
end

# Aliases {{{1

# ls {{{2
alias ls 'command ls -XF --color=auto --time-style="+'\e'[33m['\e'[32m%Y-%m-%d '\e'[35m%k:%M'\e'[33m]'\e'[m"'
alias l 'ls -l'
alias la 'l -A'
alias lh 'l -h'
alias l1 'tree --dirsfirst -ChFL 1'
alias l2 'tree --dirsfirst -ChFL 2'
alias l3 'tree --dirsfirst -ChFL 3'
alias l4 'tree --dirsfirst -ChFL 4'

# coreutils {{{2
alias L less
alias c cat
alias cp 'command cp -iv'
alias eg 'egrep -I'
alias g 'grep -I'
alias mv 'command mv -iv'
alias rm 'command rm -ivd'
alias x xargs

# others
alias psg 'ps aux | g'
alias 2pdf 'libreoffice --headless --convert-to pdf'
alias clip 'xsel -ib'
alias gdb 'command gdb -q'
alias port '/sbin/ss -ntlp'
alias r ruby
alias rsync 'command rsync --progress --partial'
alias t task
alias v 'vim --servername GVIM --remote-tab-silent'
alias wgetpaste 'command wgetpaste -X'

# Gentoo-specific {{{2
alias eme 'sudo emerge'
alias ei 'eix -uI --only-names'
function eiu
  begin
    set -lx FORMAT '<installedversions:I>'
    set -lx I '<category>/<name>-<version>[<use>]\n'
    eix $argv
  end
end
alias disp 'sudo dispatch-conf'


# vim:sw=2 sts=2 et fdm=marker
