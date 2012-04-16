
dropboxd &

synergys --address 10.0.0.1

nitrogen --restore

urxvtd &

emacs --daemon 

xmodmap ~/.xmodmap
xcompmgr -c -C -t-5 -l-5 -r4.2 -o.55 &
#cairo-compmgr &

clipit &
#parcellite &

synapse -s &

xscreensaver &

#trayer --edge bottom --align right --SetDockType true --SetPartialStrut true \
#       --widthtype request --transparent true --tint 0x333333 --alpha 10 \
#       --height 18 --heighttype pixel &

# For gnome --------------------------------
#gnome-setting-daemon &
#nm-applet &
#gnome-volume-manager &
#eval `gnome-keyring-daemon`
#gnome-power-manager &
#gnome-panel &


# Window Manager --------------------------
#exec subtle --no-randr 2> .xstartup.errors 1> .xstartup.out
#exec awesome 2> .xstartup.errors 1> .xstartup.out
#exec awesome
#exec ck-launch-session gnome-session

#xsetroot -cursor_name left_ptr

#dbus-launch --exit-with-session xmonad 2> .xstartup.errors 1> .xstartup.out
dbus-launch --exit-with-session awesome 2> .xstartup.errors 1> .xstartup.out
#dbus-launch --exit-with-session subtle --no-randr 2> .xstartup.errors 1> .xstartup.out
