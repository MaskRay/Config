# gentoo-zh overlay fcitx-4.2.5-rc1 /etc/X11/xinit/xinitrc.d/01-input
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

# /etc/X11/xinit/xinitrc.d/80-dbus
eval `dbus-launch --sh-syntax --exit-with-session`

xset s blank
exec xmonad
