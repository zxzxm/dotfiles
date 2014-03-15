;; -*- mode: Emacs-Lisp -*-
;;---------------------------------------------------------------
;; Stuff that should load first
;;---------------------------------------------------------------
;; Last modified: <2013-05-14 09:16:52 (thermans)>
;;---------------------------------------------------------------

(defvar prelude-cache-dir (concat prelude-personal-dir "/cache/")
  "This directory houses caches and other mutable data.")

(defvar prelude-backup-dir (concat prelude-personal-dir "/backups/")
  "This directory houses backups.")

;; Turn off the silly tip-of-the-day
(defun prelude-tip-of-the-day ()
  nil)
