#!/bin/bash -e

files=$(git ls-files)
target=~

link() {
  ln -sf "$PWD/$1" "$2"
}

for f in $files; do
  if [[ "$f" =~ ^home/ ]]; then
    :
  else
    continue
  fi

  echo copying $f
  g="$target/${f/home\//}"
  mkdir -p "${g%/*}"
  if ! [[ -L "$g" ]]; then
    if [[ -f "$g" || "$f" -ot "$g" ]]; then
      if diff -q "$f" "$g"; then
        echo "identical $g"
        continue
      else
        echo "overwrite $g? [yn]"
        read option
        case $option in
          [yY]);;
          [nN])
            echo "skipping $g"
            continue;;
        esac
      fi
    fi
    link "$f" "$g"
  fi
done
