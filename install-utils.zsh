#!/bin/zsh -e

typeset -A ubuntu_pkg
ubuntu_pkg[pcre]=libpcre3-dev
ubuntu_pkg[zlib]=libz-dev
ubuntu_pkg[readline]=libreadline-dev
ubuntu_pkg[ncurses]=ncurses-dev
ubuntu_pkg[sqlite]=libsqlite3-dev
ubuntu_pkg[uuid]=uuid-dev

# versions

# prepare

DOWNLOAD=/tmp
SRC=~/.local/src

mkdir -p $DOWNLOAD
mkdir -p $SRC
cd $SRC

# zsh config

setopt extended_glob       # expand *(xx)
setopt equals              # expand =program
setopt magic_equal_subst   # expand xx=~yy

# functions

info() {
  echo "\e[1;34m++++ $*\e[m"
}

error() {
  echo "\e[1;31m!!! $*\e[m"
  exit 1
}

bin() {
  [[ -n ${commands[$1]} ]]
}

# prepend PATH

[[ $PATH =~ ~/.local/bin ]] && PATH=~/.local/bin:$PATH

# download

clone() {
    [[ -d $2 ]] || git clone $1 $2
}

clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

mkdir -p ~/Dev/Bin
clone https://github.com/hugsy/gef ~/Dev/Bin/gef
clone https://github.com/zachriggle/pwndbg ~/Dev/Bin/pwndbg
clone https://github.com/Gallopsled/pwntools ~/Dev/Bin/pwntools

# install

install_system_pkgs() {
  info apt-get install ${(v)ubuntu_pkg}
  read
}

install_haskell() {
  # stack
  curl -sSL https://get.haskellstack.org/ | sh

  # structred-haskell-mode
  stack install structured-haskell-mode

  # intero
  # git clone https://github.com/commercialhaskell/intero
  # cd intero && stack build && stack install
}

preexec() {
  echo "\e[1;33m== $2\e[m"
}

((${1+1})) && "$@"

#install_stack
#install_system_pkgs
