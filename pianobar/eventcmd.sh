#!/bin/bash

# create variables
while read L; do
    k="`echo "$L" | cut -d '=' -f 1`"
    v="`echo "$L" | cut -d '=' -f 2`"
    export "$k=$v"
done < <(grep -e '^\(title\|artist\|album\|stationName\|songStationName\|pRet\|pRetStr\|wRet\|wRetStr\|songDuration\|songPlayed\|rating\|coverArt\|stationCount\|station[0-9]*\)=' /dev/stdin) # don't overwrite $1...


case "$1" in
    songstart)

        [[ $OSTYPE == "darwin11.0" ]] && echo "/usr/local/bin/growlnotify -m \"$title by $artist\" -n pianobar \"Now Playing:"
        #[[ $OSTYPE == "darwin11.0" ]] && /usr/local/bin/growlnotify -m "$title by $artist" -n pianobar "Now Playing:"
        [[ $OSTYPE == "linux-gnu" ]]  &&  echo "naughty.notify({title = \"Now Playing:\", text = \"<span color='#A5C261'>$title</span> by <span color='#A5BAF1'>$artist</span>\"})" | awesome-client -

        echo "$title -- $artist" > $HOME/.config/pianobar/nowplaying
        ;;

    # songfinish)
    #         # scrobble if 75% of song have been played, but only if the song hasn't
    #         # been banned
    #         if [ -n "$songDuration" ] && [ "$songDuration" -ne 0 ] &&
    #                         [ $(echo "scale=4; ($songPlayed/$songDuration*100)>50" | bc) -eq 1 ] &&
    #                         [ "$rating" -ne 2 ]; then
    #                 # scrobbler-helper is part of the Audio::Scrobble package at cpan
    #                 # "pia" is the last.fm client identifier of "pianobar", don't use
    #                 # it for anything else, please
    #                 scrobbler-helper -P pia -V 1.0 "$title" "$artist" "$album" "" "" "" "$((songDuration/1000))" &
    #         fi
    #         ;;

    # songlove)
    #         kdialog --title pianobar --passivepopup "LOVING '$title' by '$artist' on '$album' on station '$stationName'" 10
    #         ;;

    # songshelf)
    #         kdialog --title pianobar --passivepopup "SHELVING '$title' by '$artist' on '$album' on station '$stationName'" 10
    #         ;;

    # songban)
    #         kdialog --title pianobar --passivepopup "BANNING '$title' by '$artist' on '$album' on station '$stationName'" 10
    #         ;;

    # songbookmark)
    #         kdialog --title pianobar --passivepopup "BOOKMARKING '$title' by '$artist' on '$album'" 10
    #         ;;

    # artistbookmark)
    #         kdialog --title pianobar --passivepopup "BOOKMARKING '$artist'" 10
    #         ;;

    *)
        if [ "$pRet" -ne 1 ]; then
            [[ $OSTYPE == 'darwin11.0' ]] && growlnotify -t "Pianobar Problem: " -m "$1 failed: $pRetStr" -n pianobar
            [[ $OSTYPE == 'linux-gnu' ]]  && echo "naughty.notify({title = \"pianobar\", text = \"$1 failed: $pRetStr\"})" | awesome-client -
        elif [ "$wRet" -ne 1 ]; then
            [[ $OSTYPE == 'darwin11.0' ]] && growlnotify -t "Pianobar Problem: " -m "$1 failed: Network error: $wRetStr" -n pianobar
            [[ $OSTYPE == 'linux-gnu' ]]  && echo "naughty.notify({title = \"pianobar\", text = \"$1 failed: Network error: $wRetStr\"})" | awesome-client -
        fi
        ;;
esac
