#!/bin/zsh -e

typeset -A ubuntu_pkg
ubuntu_pkg[pcre]=libpcre3-dev
ubuntu_pkg[zlib]=libz-dev
ubuntu_pkg[readline]=libreadline-dev
ubuntu_pkg[ncurses]=ncurses-dev
ubuntu_pkg[sqlite]=libsqlite3-dev
ubuntu_pkg[uuid]=uuid-dev

# versions

RUBY_VERSION=2.2.0
NODE_VERSION=0.10

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

# install

install_system_pkgs() {
  info apt-get install ${(v)ubuntu_pkg}
  read
}

install_xstow() {
  bin xstow && return
  wget -C http://sourceforge.net/projects/xstow/files/xstow-1.0.2.tar.bz2/download -O $DOWNLOAD/xstow.tar.bz2
  tar xf $DOWNLOAD/xstow*.tar.bz2
  pushd xstow-*(/)
  ./configure --prefix=~/.local/stow/xstow
  make install
  ~/.local/stow/xstow/bin/xstow -d ~/.local/stow xstow
  popd
}

install_task() {
  bin task && return
  git clone https://git.tasktools.org/scm/tm/task.git
  info dependency: uuid-dev
  pushd task
  cmake -DCMAKE_MAKE_PROGRAM=make -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=~/.local/stow/task
  make install
  popd
  xstow -d ~/.local/stow task
  git clone https://git.tasktools.org/scm/ex/tasksh.git
  pushd tasksh
  cmake -DCMAKE_MAKE_PROGRAM=make -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=~/.local/stow/tasksh
  make install
  popd
  xstow -d ~/.local/stow tasksh
}

install_xmobar() {
  bin xmobar && return
  bin ghc || error please install ghc
  bin cabal || error please install cabal-install
  cabal update
  cabal install xmonad
  cabal install xmonad-contrib --flags='use_xft'
  cabal install xmobar --flags='with_xft with_xpm'
}

install_llpp() {
  bin llpp && return
  bin opam || error please install opam
  bin ninja || error please install ninja
  opam show lablgl || opam install lablgl
  git clone git://repo.or.cz/llpp.git
  pushd llpp
  patch -p1 <<'e'
diff --git i/build.ninja w/build.ninja
index 21812ce..2924d6d 100644
--- i/build.ninja
+++ w/build.ninja
@@ -11,3 +11,3 @@ rule cc
      depfile = $out.d
-     command = $ocamlc -cc "$cc" -ccopt "$cflags -MMD -MF $out.d -o $out" -c $in
+     command = ocamlfind ocamlopt -package lablgl -linkpkg -cc "$cc" -ccopt "$cflags -MMD -MF $out.d -o $out" -c $in
      description = cc $out
@@ -16,3 +16,3 @@ rule ocamlc
      command = env incs="$incs" pp="$pp" /bin/sh $srcdir/doocaml.sh $
-             $ocamlc $out $in -c $ocamlflags
+             ocamlopt $out $in -c $ocamlflags
      description = ocamlc $out
@@ -52,3 +52,3 @@ build $builddir/link.so: linkso $builddir/link.o
 rule link
-     command = $ocamlc -g -o $out $lablglcflags $
+     command = ocamlfind ocamlopt -package lablgl -linkpkg -g -o $out $lablglcflags $
              str$cma unix$cma lablgl$cma $in $
diff --git i/configure.sh w/configure.sh
index 56c8966..d34bf94 100644
--- i/configure.sh
+++ w/configure.sh
@@ -83,3 +83,3 @@ EOF
      echo "cma=.cmxa"
-     echo "ocamlc=ocamlopt.opt"
+     echo "ocamlc=ocamlopt"
      echo "linksocclib=-cclib"
diff --git i/doocaml.sh w/doocaml.sh
index e7c080d..962d8e4 100644
--- i/doocaml.sh
+++ w/doocaml.sh
@@ -3,3 +3,3 @@ set -e
 
-compiler="$1"
+compiler='ocamlfind ocamlopt -package lablgl'
 out="$2"
e
  sh configure.sh -n . # use ocamlopt
  ninja
  cp -a build/llpp ~/.local/bin/
}

install_urxvt_perls() {
  [[ -d $SRC/urxvt_perls ]] && return
  git clone https://github.com/muennich/urxvt-perls
  info change path URxvt.perl-lib:
  sed -i "/^URxvt.perl-lib/s/ray/$USER/" ~/.Xresources
}

install_rvm() {
  bin rvm && return
  curl -sSL https://get.rvm.io | bash -s -- --autolibs=read-fail --ignore-dotfiles
  . ~/.rvm/scripts/rvm
  rvm install $RUBY_VERSION
  rvm use --default $RUBY_VERSION
}

install_nvm() {
  [[ -e ~/.nvm/nvm.sh ]] && return
  curl -sSL https://raw.github.com/creationix/nvm/master/install.sh | bash
  . ~/.nvm/nvm.sh
  nvm install $NODE_VERSION
  nvm alias default $NODE_VERSION
}

install_lnav() {
  bin lnav && return
  git clone https://github.com/tstack/lnav
  pushd lnav
  ./configure --prefix=~/.local/stow/lnav
  make install
  popd
  xstow -d ~/.local/stow lnav
}

install_jq() {
  # download binary
  wget https://stedolan.github.io/jq/download/linux64/jq -O ~/.local/bin/jq
}

preexec() {
  echo "\e[1;33m== $2\e[m"
}

install_system_pkgs
install_xstow
install_task
install_xmobar
install_llpp
install_urxvt_perls
install_rvm
install_nvm
install_lnav
install_jq
