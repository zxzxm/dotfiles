;; -*- mode: Emacs-Lisp; fill-column: 75; comment-column: 50; -*-
;;----------------------------------------------------------------
;; Assorted languages
;;----------------------------------------------------------------
;; Last modified: <2013-02-11 16:37:16 (thermans)>
;;----------------------------------------------------------------

;;; ............................................................ &ruby ...
;; Override for ruby-tools (helm conflict)
(defvar ruby-tools-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "C-'") 'ruby-tools-to-single-quote-string)
    (define-key map (kbd "C-\"") 'ruby-tools-to-double-quote-string)
    (define-key map (kbd "C-:") 'ruby-tools-to-symbol)
    (define-key map (kbd "C-;") 'ruby-tools-clear-string)
    (define-key map (kbd "#") 'ruby-tools-interpolate)
    map)
  "Keymap for `ruby-tools-mode'.")

;; Rsense (http://cx4a.org/software/rsense/index.html) ----------
(add-to-list 'load-path (concat prelude-vendor-dir "/rsense"))
(setq rsense-home (concat prelude-vendor-dir "/rsense"))
(add-to-list 'load-path (concat rsense-home "/etc"))
(require 'rsense)

(add-hook 'ruby-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-rsense-method)
            (add-to-list 'ac-sources 'ac-source-rsense-constant)))

(add-hook 'ruby-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c h") 'rsense-type-help)))

(defun tim-ruby-mode-hook ()
  (local-set-key [return] 'reindent-then-newline-and-indent)
  (setq indent-tabs-mode nil))

(add-hook 'ruby-mode-hook 'tim-ruby-mode-hook)

;; Puppet -------------------------------------------------------
(add-to-list 'load-path (concat prelude-vendor-dir "/puppet"))
(autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests")
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;;; ............................................................ &vala ...

(add-to-list 'auto-mode-alist '("\\.vala$" . vala-mode))
(add-to-list 'auto-mode-alist '("\\.vapi$" . vala-mode))
(add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.vapi$" . utf-8))
