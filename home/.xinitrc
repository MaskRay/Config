# source xinitc.d scripts
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

export BROWSER=chrome
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

xset s off dpms 600 600 600
xset r rate 200 50
xset m 3 3
exec xmonad

# tap once: click
# two twice: right click
# two thice middle click
synclient TapButton1=1 TapButton2=3 TapButton3=2

# speed
synclient MinSpeed=1.5 MaxSpeed=3.5 AccelFactor=0.1

# scrolling using two fingers
synclient VertTwoFingerScroll=1 HorizTwoFingerScroll=1
