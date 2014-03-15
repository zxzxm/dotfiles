;; -*- mode: Emacs-Lisp -*-
;;---------------------------------------------------------------
;; Dired Stuff
;;---------------------------------------------------------------
;; Last modified: <2014-03-14 16:04:31 thermans>
;;---------------------------------------------------------------

(require 'dired-x)

;; Run Dired in a single buffer
(autoload 'dired-single-buffer "dired-single" "" t)
(autoload 'dired-single-buffer-mouse "dired-single" "" t)
;; (autoload 'dired-single-magic-buffer "dired-single" "" t)
;; (autoload 'dired-single-toggle-buffer-name "dired-single" "" t)

(defun my-single-buffer ()
  (interactive)
  (dired-single-buffer ".."))

(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map "^" 'my-single-buffer)
  )

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    (my-dired-init)
  (add-hook 'dired-load-hook 'my-dired-init))

;; Add a sort menu for dired
(add-hook 'dired-load-hook
          (lambda () (require 'dired-sort-menu)))

(add-hook 'dired-mode-hook
          (function (lambda ()
                      ;; Omit uninteresting files ("..", "*.elc", etc.)
                      (dired-omit-mode 1)
                      )))
;; dired-details
(require 'dired-details)
(dired-details-install)
