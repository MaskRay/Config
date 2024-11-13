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
set -x LESS '-FiMRwX --shift 5 -z-4'
set -x GREP_OPTIONS '--color=auto'
set -x MENUCONFIG_COLOR blackbg

if command -q dircolors  # GNU coreutils
  set GNU 1
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
alias l 'eza -l'
alias la 'eza -lA'
alias ls eza

# coreutils {{{2
abbr -a --position anywhere L '| less'
alias c cat
alias cp 'cp -iv'
alias mv 'mv -iv'
if set -q GNU
  alias rm 'rm -iv --one-file-system'
else
  alias rm 'rm -iv'
end

# git {{{2
abbr ga 'git add'
abbr gau 'git add -u'
abbr gb 'git branch'
abbr gcl 'git clone'
abbr gco 'git checkout'
abbr gd 'git diff'
abbr gdc 'git diff --cached'
abbr gl 'git l'
abbr glp 'git l -p'
abbr gpl 'git pull'
abbr gpu 'git push'
abbr gs 'git switch'
abbr gst 'git status'

## others
alias psg 'ps aux | g'
alias 2pdf 'libreoffice --headless --convert-to pdf'
alias clip 'xsel -ib'
alias gdb 'command gdb -q'
alias port '/sbin/ss -ntlp'
alias py python
alias rb ruby
alias rsync 'rsync --progress --partial'
alias t task

alias e="nvr --servername=$XDG_RUNTIME_DIR/nvim.pipe --remote"
alias vs="nvr --servername=$XDG_RUNTIME_DIR/nvim.pipe --remote -O"
alias t="nvr --servername=$XDG_RUNTIME_DIR/nvim.pipe --remote-tab"

## systemd {{{2
alias sy='sudo systemctl'
alias syu='systemctl --user'

## Arch Linux {{{2
alias pD='sudo pacman -D'
alias yS='yay -S'
alias ySs='yay -Ss'
alias ySyu='yay -Syua --noconfirm'
alias pSy='sudo pacman -Sy'
alias pSyu='sudo pacman -Syu --noconfirm' # Synchronize with repositories and then upgrade packages that are out of date on the local system.
alias pS='sudo pacman -S'                 # Install specific package(s) from the repositories
alias pU='sudo pacman -U'                 # Install specific package not from the repositories but from a file
alias pR='sudo pacman -R'                 # Remove the specified package(s), retaining its configuration(s) and required dependencies
alias pRns='sudo pacman -Rns'             # Remove the specified package(s), its configuration(s) and unneeded dependencies
alias pSi='pacman -Si'                    # Display information about a given package in the repositories
alias pSs='pacman -Ss'                    # Search for package(s) in the repositories
alias pQi='pacman -Qi'                    # Display information about a given package in the local database
alias pQs='pacman -Qs'                    # Search for package(s) in the local database
alias paclo="pacman -Qdt"                 # List all packages which are orphaned
alias pacc="sudo pacman -Scc"             # Clean cache - delete all not currently installed package files
alias pQl="pacman -Ql"                    # List all files installed by a given package
alias pQo="pacman -Qo"
alias pacexp="sudo pacman -D --asexp"     # Mark one or more installed packages as explicitly installed
alias pacimp="sudo pacman -D --asdep"     # Mark one or more installed packages as non explicitly installed

if type -q jj
  jj util completion fish | source
end

# vim:sw=2 sts=2 et fdm=marker
