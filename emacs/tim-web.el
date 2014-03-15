;; -*- mode: Emacs-Lisp; fill-column: 75; comment-column: 50; -*-
;;---------------------------------------------------------------
;; Web configuration
;;---------------------------------------------------------------
;; Last Saved Last modified: <2013-10-08 11:20:42 (thermans)>
;;----------------------------------------------------------------

;; Load nxhtml
;;(load (concat prelude-vendor-dir "/nxhtml/autostart.el"))

;;-------------------------------------------------------------
;; Web-mode customizations
;;
;; Override this function to let web-mode syntax coloring work
(defun prelude-web-mode-defaults ()
  ;; Disable whitespace-mode when using web-mode
  (whitespace-mode -1)
  (rainbow-mode -1)
  (font-lock-mode -1)
  (turn-on-evil-matchit-mode)
  ;; Customizations
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-disable-autocompletion t)
  (local-set-key (kbd "RET") 'newline-and-indent))
