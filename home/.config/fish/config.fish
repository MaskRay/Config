set PATH ~/bin ~/.local/bin ~/.cargo/bin ~/.nimble/bin $PATH

# Functions {{{1
function match
  echo $$argv[2] | grep -q $argv[1]
end

function md
  mkdir -p $argv[1]
  cd $argv[1]
end

# Environment variables {{{1
set -x EDITOR nvim
set -x LESS '-FiMRwX --shift 5 -z-4'
set -x GREP_OPTIONS '--color=auto'
set -x MENUCONFIG_COLOR blackbg
set -x RIPGREP_CONFIG_PATH $HOME/.ripgreprc
set -x MANPAGER 'nvim +Man!'

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
  set st $status
  if test $status -eq 0
    set st
  end
  set reset \e'[m'
  set red \e'[31m'
  set blue \e'[1m'\e'[34m'
  set magenta \e'[1m'\e'[35m'
  set cyan \e'[1m'\e'[36m'
  set white \e'[37m'

  switch $USER
    case root
      set user_prompt '#'
    case '*'
      set user_prompt '%'
  end

  echo $blue\u256d\u2500$cyan(whoami) $white@ $magenta(prompt_hostname) $white'>>=' $cyan(prompt_pwd) $reset(fish_git_prompt) $reset$st
  echo -n $blue\u2570\u2500$red$user_prompt(set_color normal)' '
end

# Bindings {{{1

# Aliases {{{1

alias ... 'cd ../..'
alias .... 'cd ../../..'
alias ..... 'cd ../../../..'
function last_history_item
  echo $history[1]
end
abbr -a !! --position anywhere --function last_history_item
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
alias fd 'fd --hidden --no-ignore'
abbr ob objdump
abbr re readelf -W
abbr G -p anywhere '| grep -E'

abbr ni ninja
alias rg 'rg -u'

# git {{{2
alias ga 'git add'
alias gau 'git add -u'
alias gb 'git branch'
alias gc 'git commit'
alias gcl 'git clone'
alias gco 'git checkout'
alias gd 'git diff'
alias gdc 'git diff --cached'
alias gl 'git l'
alias glp 'git l -p'
alias gpl 'git pull'
alias gpu 'git push'
alias grs 'git restore'
alias gs 'git switch'
alias gst 'git status'

## others
alias +x 'chmod +x'
alias psg 'ps aux | grep -E'
alias 2pdf 'libreoffice --headless --convert-to pdf'
alias clip 'xsel -ib'
alias gdb 'command gdb -q'
alias port '/sbin/ss -ntlp'
abbr py python
abbr rb ruby
alias rsync 'rsync --progress --partial'
alias xst 'xstow -d ~/.local/stow'

alias e "nvr --servername=$XDG_RUNTIME_DIR/nvim.pipe --remote"
alias vs "nvr --servername=$XDG_RUNTIME_DIR/nvim.pipe --remote -O"
alias t "nvr --servername=$XDG_RUNTIME_DIR/nvim.pipe --remote-tab"

## systemd {{{2
abbr sy 'sudo systemctl'
abbr syu 'systemctl --user'

## Arch Linux {{{2
abbr pD 'sudo pacman -D'
abbr yS 'yay -S'
abbr ySs 'yay -Ss'
abbr ySyu 'yay -Syua --noconfirm'
abbr pSy 'sudo pacman -Sy'
abbr pSyu 'sudo pacman -Syu --noconfirm' # Synchronize with repositories and then upgrade packages that are out of date on the local system.
abbr pS 'sudo pacman -S'                 # Install specific package(s) from the repositories
abbr pU 'sudo pacman -U'                 # Install specific package not from the repositories but from a file
abbr pR 'sudo pacman -R'                 # Remove the specified package(s), retaining its configuration(s) and required dependencies
abbr pRns 'sudo pacman -Rns'             # Remove the specified package(s), its configuration(s) and unneeded dependencies
abbr pSi 'pacman -Si'                    # Display information about a given package in the repositories
abbr pSs 'pacman -Ss'                    # Search for package(s) in the repositories
abbr pQi 'pacman -Qi'                    # Display information about a given package in the local database
abbr pQs 'pacman -Qs'                    # Search for package(s) in the local database
abbr paclo "pacman -Qdt"                 # List all packages which are orphaned
abbr pacc "sudo pacman -Scc"             # Clean cache - delete all not currently installed package files
abbr pQl "pacman -Ql"                    # List all files installed by a given package
abbr pQo "pacman -Qo"
abbr pacexp "sudo pacman -D --asexp"     # Mark one or more installed packages as explicitly installed
abbr pacimp "sudo pacman -D --asdep"     # Mark one or more installed packages as non explicitly installed

type -q fzf && fzf --fish | source

type -q jj && jj util completion fish | source

for i in /tmp/Debug/bin/*
  alias my(string split -r -m1 / $i)[2] $i
  alias rr(string split -r -m1 / $i)[2] "rr record $i"
end
alias myob=/tmp/Debug/bin/llvm-objdump
alias mylit=/tmp/Debug/bin/llvm-lit
alias rrob="rr record /tmp/Debug/bin/llvm-objdump"

for i in /tmp/Rel/bin/*
  alias f(string split -r -m1 / $i)[2] $i
end
alias fob=/tmp/Rel/bin/llvm-objdump
alias flit=/tmp/Rel/bin/llvm-lit

alias fobj "fob --no-addresses --no-show-raw-insn -M intel --symbolize-operands"

if test -d ~/Util/z.lua && command -q lua
  lua ~/Util/z.lua/z.lua --init fish | source
end

if test -f ~/.config/fish/conf.d/fnm.fish
  source ~/.config/fish/conf.d/fnm.fish
end
if test -f ~/.config/fish/local.fish
  source ~/.config/fish/local.fish
end

# vim:sw=2 sts=2 et fdm=marker
