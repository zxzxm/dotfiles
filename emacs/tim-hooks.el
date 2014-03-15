;; -*- mode: Emacs-Lisp; fill-column: 75; comment-column: 50; -*-
;;----------------------------------------------------------------
;; Hooks
;;----------------------------------------------------------------
;; Last modified: <2012-08-13 17:17:39 (thermans)>
;;----------------------------------------------------------------

;; Rainbow mode
(add-hook 'sass-mode-hook (lambda () (rainbow-mode 1)))
(add-hook 'scss-mode-hook (lambda () (rainbow-mode 1)))
