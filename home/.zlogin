if [[ -z $DISPLAY && $XDG_VTNR -eq 1 ]]; then
  xinit -- -nolisten tcp :0 vt$XDG_VTNR
fi
