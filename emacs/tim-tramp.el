;; -*- mode: Emacs-Lisp; fill-column: 75; comment-column: 50; -*-
;;---------------------------------------------------------------
;; Tramp configuration
;;---------------------------------------------------------------
;; Last Saved Last modified: <2013-05-14 09:24:28 (thermans)>
;;----------------------------------------------------------------

;; Handle backups
(setq tramp-backup-directory-alist nil)
(setq tramp-auto-save-directory (concat prelude-savefile-dir "/auto-saves/"))

(add-to-list 'backup-directory-alist
                  (cons tramp-file-name-regexp (concat prelude-backup-dir "tramp/")))

;; Securely save passwords
;;(require 'auth-source)
;;(setq auth-sources (quote ((:source (concat prelude-savefile-dir "/tramp-authinfo") :host t :protocol t))))

;; This prevents tramp from having problems writing remote files
(setq file-precious-flag nil)

(require 'tramp)
(setq tramp-debug-buffer t)
(setq tramp-verbose 9)

;; Defaults
(setq tramp-default-method "ssh")
(setq tramp-default-user "therma000")

(setq password-cache-expiry nil)
(setq tramp-persistency-file-name (concat prelude-savefile-dir "/tramp-history"))
;; (setq tramp-remote-path (quote ("/bin" "/usr/bin" "/usr/sbin" "/usr/local/bin" "/usr/ccs/bin" "/usr/xpg4/bin")))

;;(setq tramp-default-host "jumphost")

;;(setq tramp-default-proxies-alist (quote (("." "nil" "/ssh:thermans@jumphost:"))))
;;(add-to-list 'tramp-default-proxies-alist
;;           '("\\." nil "/ssh:hermans.us:"))

;; Show the host-name in the mode-line
(defconst my-mode-line-buffer-identification
  (list
   '(:eval
          (let ((host-name
                 (if (file-remote-p default-directory)
                  (tramp-file-name-host
                   (tramp-dissect-file-name default-directory))
                  (system-name))))
           (if (string-match "^[^0-9][^.]*\\(\\..*\\)" host-name)
            (substring host-name 0 (match-beginning 1))
            host-name)))
   ": %12b"))

 (setq-default
  mode-line-buffer-identification
  my-mode-line-buffer-identification)

 (add-hook
  'dired-mode-hook
  '(lambda ()
         (setq
          mode-line-buffer-identification
          my-mode-line-buffer-identification)))
