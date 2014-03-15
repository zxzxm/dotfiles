;; -*- mode: Emacs-Lisp; fill-column: 75; comment-column: 50; -*-
;;---------------------------------------------------------------
;; Auto-Complete mode
;;---------------------------------------------------------------
;; Last modified: <2014-03-14 17:03:44 thermans>
;;---------------------------------------------------------------

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat prelude-personal-dir "/auto-complete/dict"))
(setq ac-user-dictionary-files (quote ((concat prelude-savefile-dir "/ac-common-dict"))))
(setq ac-comphist-file (concat prelude-savefile-dir "/ac-comphist.dat"))
(global-auto-complete-mode t)
(setq ac-auto-start 3)
(setq ac-dwim t)
(ac-config-default)
(ac-flyspell-workaround)
