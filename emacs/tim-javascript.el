;; -*- mode: Emacs-Lisp; fill-column: 75; comment-column: 50; -*-
;;----------------------------------------------------------------
;; Javascript configuration (https://github.com/thomblake/js3-mode)
;;----------------------------------------------------------------
;; Last modified: <2013-04-04 08:55:28 (thermans)>
;;----------------------------------------------------------------

(add-to-list 'load-path (concat prelude-vendor-dir "/js2-mode/"))

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))


;;;; js3-mode -----------------------------------------------------
;; (add-to-list 'load-path (concat prelude-vendor-dir "/js3-mode/"))

;; ;; To override an error
;; (defvar *javascript-mode-syntax-table* nil "placeholder")

;; (load (concat prelude-vendor-dir "/js3-mode/js3"))

;; (setq js3-auto-indent-p t)         ; it's nice for commas to right themselves.
;; (setq js3-enter-indents-newline t) ; don't need to push tab before typing
;; (setq js3-indent-on-enter-key t)   ; fix indenting before moving on

;; (add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))
