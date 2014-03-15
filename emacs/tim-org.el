;; -*- mode: Emacs-Lisp; fill-column: 75; comment-column: 50; -*-
;;---------------------------------------------------------------
;; Org-mode configuration
;;---------------------------------------------------------------
;; Last Saved Last modified: <2014-02-07 00:52:58 thermans>
;;----------------------------------------------------------------

(require 'org-protocol)

(setq org-directory "~/Dropbox/org")

;; Capture settings
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates (quote (
                                    ("n" "Notes" entry (file "~/Dropbox/org/notes.org") "* %U

%?

")
                                    ("u" "uri" entry (file+headline "~/Dropbox/org/urls.org" "Some Default Headline for captures")
   "*** %^{Title}\n\n    Source: %u, %c\n    %i")
)))


;; (setq org-capture-templates
;;       '(("t" "Todo" entry (file+headline (concat org-directory "/todo.org") "Tasks")
;;          "* TODO %?\n  %i\n  %a")
;;         ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
;;          "* %?\nEntered on %U\n  %i\n  %a")))

(define-key prelude-mode-map "\C-cn" nil)
(define-key global-map "\C-cn"
  (lambda () (interactive) (org-capture nil "n")))
