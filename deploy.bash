#!/bin/bash -e

files=$(git ls-files)
target=~

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
      if ! diff -q "$f" "$g"; then
	vimdiff "$f" "$g"
      fi
    fi
    ln -sf "$PWD/$f" "$g"
  fi
done
