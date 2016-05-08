# Config

Ray's comprehensive configuration archive.

## Installation

```
pacman -S stack
stack install fast-tags ghc-mod hoogle hscope hledger pointfree pointful
```

## Features

Default applications:

See `home/.local/share/applications/mimeapps.list` and related desktop entries.

Gentoo Portage:

- collected many utilities to facilitate day-to-day shell usage (notable: GNU Parallel, ImageMagick, Unison, renameutils, TaskWarrior)
- servers/clients/tools related to network (MongoDB, ProFTPd, Nginx, PostgreSQL, Redit, ...)
- programming related application collections including compilers (GHC, OCaml, SBCL, Guile, Node.js, GNU Smalltalk, Erlang, Vala, etc)
  and many development tools

Zsh:

- use fasd to navigate the filesystem hierarchy
- numerous aliases
- decent autocomplete settings

Vim:

- Haskell, Ruby, Node.js, Python, C++ and Web development settings
- lots of key bindings
- Many goodies including [Global](www.gnu.org/software/global), Ack, EasyMotion, CtrlP, Syntastic, UltiSnips, Tabular

XMonad:

- organize applications on topic basic
- a great many bindings (perhaps over one hundred) for window arrangement, x11 utilities and so on
- scratchpads for ghci, ocaml, coffee, erl (Erlang), node (Node.js), R, pry (Ruby), ipython, lua, gst (GNU Smalltalk), task (taskwarrior), alsa-mixer, etc
- direction-based navigation via XMonad.Actions.Navigation2D
- website launcher for wikipedia, google, duckduckgo, github, developer.mozilla.org, etc

X resources:

- XTerm
- URxvt
- XScreenSaver
- ...

Systemd services:

- [kmscon](https://github.com/dvdhrm/kmscon)

Others:

- Mutt
- GDB
- Tmux
- Mailcap
- Pentadactyl (for Firefox)
- Udev
- Xorg
- ...

Have a look at my [Linux desktop config (in Chinese)](http://maskray.me/portfolio/linux-desktop) for my choice of desktop applications.

## Acknowledgements

- [@pyx](https://github.com/pyx) (Philip Xu)
  My config absorbs quite a few ideas from his well-organized config.
  He is also my abecedarian leading me to discover the mysterious FOSS world.
  Thank you, Philip!
- [@adam8157](https://github.com/adam8157) (Adam Lee)
- [@roylez](https://github.com/roylez/dotfiles) (Roy Zuo)
- [@laurentb](https://github.com/laurentb/public-dotfiles) (Laurent Bachelier)
- [@terlar](https://github.com/terlar/dotfiles) (Terje Larsen)
