;; -*- mode: Emacs-Lisp; fill-column: 75; comment-column: 50; -*-
;;----------------------------------------------------------------
;; Helm (anything) configuration (https://github.com/emacs-helm)
;;----------------------------------------------------------------
;; Last modified: <2013-10-29 10:05:33 (thermans)>
;;----------------------------------------------------------------
(require 'helm-config)
(require 'helm-buffers)
(require 'helm-files)
(require 'helm-imenu)
(require 'helm-bookmark)
(require 'helm-man)
(require 'helm-info)
;; (global-set-key (kbd "C-'") 'helm-mini)
;; (helm-mode 1)

;; Replaces describe-bindings.  Do "C-h b" or "C-x C-h"
(require 'helm-descbinds)
(helm-descbinds-mode)

;; Helm for rails
;; (add-to-list 'load-path (concat prelude-vendor-dir "/helm-on-rails"))
;; (require 'helm-rails)

;; (defun my-railshelm ()
;;   (interactive)
;;   (helm-other-buffer
;;    '(helm-c-source-rails-project-files)
;;    " *my-railshelm*"
;;    )
;;   )
;; (global-set-key (kbd "C-;") 'my-railshelm)


(defun my-helm ()
  (interactive)
  (helm-other-buffer
   '(
     helm-c-source-buffers-list
     helm-c-source-bookmarks
     helm-c-source-recentf
     helm-c-source-file-name-history
     helm-c-source-etags-select
     helm-c-source-file-cache
     helm-c-source-files-in-current-dir
     helm-c-source-locate
     )
   " *my-helm*"))
(global-set-key (kbd "C-'") 'my-helm)

(defun my-helm-info ()
  (interactive)
  (helm-other-buffer
   '(
     helm-c-source-man-pages
     helm-c-source-info-pages
     )
   " *my-helm-info*"))
(global-set-key (kbd "C-c '") 'my-helm-info)

;; Helm swoop
;; (global-set-key (kbd "M-i") 'helm-swoop)
;; (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
;; (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
;; (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
(define-key isearch-mode-map (kbd "C-x M-i") 'helm-multi-swoop-all-from-isearch)

;; ;; Save buffer when helm-multi-swoop-edit complete
;; (setq helm-multi-swoop-edit-save t)

