export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export GTK_IM_MODULE=xim
export QT_IM_MODULE=xim
export XMODIFIERS=@im=fcitx

xset s blank
eval `dbus-launch --sh-syntax --exit-with-session`
exec xmonad
