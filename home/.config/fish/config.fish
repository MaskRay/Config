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

function md
  mkdir -p $argv[1]
  cd $argv[1]
end

# Environment variables {{{1
set -x LESS '-MRi --shift 5'
set -x GREP_OPTIONS '--color=auto'
set -x MENUCONFIG_COLOR blackbg

# Set LS_COLORS {{{2
if command -q dircolors  # GNU coreutils
  set GNU 1
end
if set -q GNU
  if match 256color TERM; and [ -f ~/.lscolor256 ]
    eval (dircolors -c ~/.lscolor256 | sed 's/env/ -x/')
  else if [ -f ~/.lscolor ]
    eval (dircolors -c ~/.lscolor | sed 's/env/ -x/')
  end
end

# Theme {{{1
set fish_color_command yellow
set fish_color_param white
set fish_color_error red -bold

# Greeting {{{2
set fish_greeting ''

# Prompt {{{2
set fish_prompt_pwd_dir_length 0
function fish_prompt
  set reset \e'[m'
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

  echo $blue\u256d\u2500$cyan(whoami) $white@ $magenta$__fish_prompt_hostname $white'>>=' $cyan(prompt_pwd) $reset(fish_git_prompt)
  echo -n $blue\u2570\u2500$red$user_prompt(set_color normal)' '
end

# Bindings {{{1

# Aliases {{{1

# ls {{{2
if set -q GNU
  alias ls 'ls -XF --color=auto --time-style="+'\e'[33m['\e'[32m%Y-%m-%d '\e'[35m%k:%M'\e'[33m]'\e'[m"'
else
  alias ls 'ls -F'
end
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
alias cp 'cp -iv'
alias eg 'egrep -I'
alias g 'grep -I'
alias mv 'mv -iv'
if set -q GNU
  alias rm 'rm -iv --one-file-system'
else
  alias rm 'rm -iv'
end
alias x xargs

# git {{{2
alias ga 'git add'
alias gau 'git add -u'
alias gb 'git branch'
alias gcl 'git clone'
alias gco 'git checkout'
alias gd 'git diff'
alias gl 'git l'
alias glp 'git l -p'
alias gpl 'git pull'
alias gpu 'git push'
alias gs 'git status'

# others
alias psg 'ps aux | g'
alias 2pdf 'libreoffice --headless --convert-to pdf'
alias clip 'xsel -ib'
alias gdb 'command gdb -q'
alias port '/sbin/ss -ntlp'
alias r ruby
alias rsync 'rsync --progress --partial'
alias t task

# vim:sw=2 sts=2 et fdm=marker
