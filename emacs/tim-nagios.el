;; -*- mode: Emacs-Lisp; fill-column: 75; comment-column: 50; -*-
;;----------------------------------------------------------------
;; Nagios mode (http://michael.orlitzky.com/code/nagios-mode.php)
;;----------------------------------------------------------------
;; Last modified: <2012-12-11 17:15:59 (thermans)>
;;----------------------------------------------------------------

(add-to-list 'load-path (concat prelude-vendor-dir "/nagios-mode"))

(autoload 'nagios-mode "nagios-mode" nil t)

(setq auto-mode-alist
      (append (list '("\\.cfg$" . nagios-mode))
              auto-mode-alist))
