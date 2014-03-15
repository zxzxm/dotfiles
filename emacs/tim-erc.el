;; -*- mode: Emacs-Lisp; fill-column: 75; comment-column: 50; -*-
;;---------------------------------------------------------------
;; Erc configuration
;;---------------------------------------------------------------
;; Last Saved Last modified: <2014-01-27 10:34:52 thermans>
;;----------------------------------------------------------------

(setq erc-nick "thermans")

(setq erc-log-channels-directory "~/Dropbox/im/erc/")

(setq erc-hide-list '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))

(defun start-irc ()
  "Connect to IRC."
  (interactive)
  (when (y-or-n-p "Do you want to start IRC? ")
    (erc :server "cloud-irc.comcast.net" :port 6667 :nick erc-nick)))
