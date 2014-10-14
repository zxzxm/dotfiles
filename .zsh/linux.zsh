# Linux specific stuff -------------------------------------------
if [[ $OSTYPE == 'linux-gnu' ]]; then

    # For Java apps
    export _JAVA_AWT_WM_NONREPARENTING=1
    export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'

    # For MPD clients
    export MPD_HOST=/home/thermans/.mpd/socket

    xmodmap ~/.xmodmap

    # Proxy
    if [[ $HOST == 'nkosi' ]]; then
    	#export http_proxy="http://127.0.0.1:10000"
    	#export https_proxy="http://127.0.0.1:10000"
    fi


    # List all installed packages
    function pkgs () {
        yaourt -Qei $(yaourt -Qu|cut -d" " -f 1)|  \
            awk ' BEGIN {FS=":"}/^Name/{printf("\033[1;34m%s\033[0;37m", $2)}/^Description/{print $2}'
        # yaourt -Qei $(yaourt -Qu|cut -d" " -f 1)|awk ' BEGIN {FS=":"}/^Name/{printf("%s", $2)}/^Description/{print $2}'
    }

    # Show details of an package
    function pkg () {
        test -z "$1" && echo "usage: pkg packagename" && return
        yaourt -Qi $1
    }

    # Find orphaned packages
    function orphans() {
        if [[ ! -n $(pacman -Qdt) ]]; then
            echo "No orphans to remove."
        else
            sudo pacman -Rs $(pacman -Qdtq)
        fi
    }
fi 
