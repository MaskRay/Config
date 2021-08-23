#!/usr/bin/env zsh
set -e

files=($(git ls-files | egrep -v 'backup|.ssh|proxy.pac.coffee|weechat'))
target=~
LN_OPT=-sf
[[ $(uname) =~ Linux && $(ln --version) =~ coreutils ]] && LN_OPT=-sfr

declare -A dir
dir[.vim]=1
#dir[.emacs.d/private/+my]=1
dir[.config/doom]=1

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
  ln "$LN_OPT" "$PWD/$1" "$2"
}

do_ssh() {
  #cp -auT home/.ssh ~/.ssh
  if [[ -d ~/.ssh ]]; then
    chmod 700 ~/.ssh
    chmod 600 ~/.ssh/*
  fi
}

do_mkdir() {
  [[ -f ~/.history ]] && rm -i ~/.history
  mkdir -p ~/{.history,tmp}
  mkdir -p ~/.vimtmp/{backup,swap,undo}
  mkdir -p ~/Wallpapers ~/.local/opt
}

do_git() {
  git submodule update --init --recursive
  mkdir -p ~/.vim/bundle
  if [[ ! -d ~/.vim/bundle/vim-plug ]]; then
    git clone https://github.com/junegunn/vim-plug ~/.vim/bundle/vim-plug
  fi
}

do_mkdir
do_ssh
do_git

for f in "${(@k)dir[@]}"; do
  g="$target/${f/home\//}"
  mkdir -p "${g%/*}"
  if ! [[ -L "$g" ]]; then
    if [[ -e "$g" ]]; then
      action "$g exists"
      continue
    fi
    link "home/$f" "$g"
  fi
done

for f in ${files[@]}; do
  if [[ "$f" =~ ^home/ ]]; then
    ff=${f/home\//}
    skip=
    while :; do
      [[ ${dir[$ff]+_} ]] && skip=1
      [[ $ff =~ / ]] || break
      ff=${ff%/*}
    done
    [[ -n $skip ]] && continue
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
        while :; do
          warning "Overwrite $g ?\n(y)es (n)o (m) vim -d (q)uit [y/n/m/q]"
          read -rs 'option?Option: '
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
