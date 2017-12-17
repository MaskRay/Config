#!/usr/bin/env bash
set -e

files=($(git ls-files | egrep -v 'backup|.ssh|proxy.pac.coffee|weechat'))
target=~

info() {
  printf "\e[1;36m$*\e[m\n"
}

action() {
  printf "\e[1;32m$*\e[m\n"
}

warning() {
  printf "\e[1;33m$*\e[m\n"
}

link() {
  ln -sf "$PWD/$1" "$2"
}

do_ssh() {
  #cp -auT home/.ssh ~/.ssh
  chmod 700 ~/.ssh
  chmod 600 ~/.ssh/*
}

do_mkdir() {
  mkdir -p ~/{.history,tmp}
  mkdir -p ~/.vimtmp/{backup,swap,undo}
  mkdir -p ~/Wallpapers ~/.local/opt
}

do_git() {
  git submodule update --init --recursive
}

for f in ${files[@]}; do
  if [[ "$f" =~ ^home/ ]]; then
    :
  else
    continue
  fi

  info "Copying $f"
  g="$target/${f/home\//}"
  mkdir -p "${g%/*}"
  if ! [[ -L "$g" ]]; then
    if [[ -f "$g" || "$f" -ot "$g" ]]; then
      if diff -q "$f" "$g"; then
        action "identical $g"
        continue
      else
        diff -u "$g" "$f" | less -FMX
        echo aaaaa
        while :; do
          warning "Overwrite $g ?\n(y)es (n)o (m) vim -d (q)uit [y/n/m/q]"
          read -rsn 1 option
          case $option in
            [ny])
              break;;
            m)
              vim -d "$g" "$f";;
            q)
              exit;;
            *)
              continue;;
          esac
        done
        if [[ $option == n ]]; then
          action "skipping $g"
          continue
        fi
      fi
    fi
    link "$f" "$g"
  fi
done

do_ssh
do_mkdir
do_git
