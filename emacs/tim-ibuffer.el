;; -*- mode: Emacs-Lisp; fill-column: 75; comment-column: 50; -*-
;;----------------------------------------------------------------
;; iBuffer configuration
;;----------------------------------------------------------------
;; Last modified: <2012-08-13 17:18:08 (thermans)>
;;----------------------------------------------------------------

(require 'ibuffer)

(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-show-empty-filter-groups nil)

(setq ibuffer-saved-filter-groups
      (quote (
              ("default"
               ("dired   " (mode . dired-mode))
               ("shell   " (or
                            (mode . eshell-mode)
                            (mode . shell-mode)))
               ("music   " (or
                            (name . "^\\*EMMS")
                            (mode . emms-mode)))
               ("perl    " (mode . cperl-mode))
               ("ruby    " (or
                            (filename . "erb$")
                            (filename . "rhtml$")
                            (mode . ruby-mode)
                            (mode . yaml-mode)
                            (mode . inferior-ruby-mode)
                            (name . "^\\*rails\\*$")
                            (name . "^\\*RServer\\*$")))
               ("erc     " (mode . erc-mode))
               ("man     " (or
                            (mode . woman-mode)
                            (mode . man-mode)))
               ("organize" (or
                            (name . "^\\*Calendar\\*$")
                            (name . "^diary$")
                            (mode . muse-mode)
                            (mode . org-mode)
                            (mode . remember-mode)))
               ("web     " (or
                            (mode . js2-mode)
                            (mode . javascript-mode)
                            (mode . espresso-mode)
                            (filename . "js$")
                            (mode . haml-mode)
                            (mode . nxhtml-mode)
                            (mode . sass-mode)
                            (mode . scss-mode)
                            (mode . css-mode)))
               ("emacs   " (or
                            (mode . emacs-lisp-mode)
                            (name . "^\\*scratch\\*$")
                            (name . "^\\*Messages\\*$"))))
              ("rails"
               ("views    " (or
                             (filename . "erb$")
                             (filename . "html$")
                             (mode . haml-mode)))
               ("ruby     " (or
                             (mode . ruby-mode)
                             (mode . inferior-ruby-mode)
                             (name . "^\\*rails\\*$")
                             (name . "^\\*RServer\\*$")))
               ("config   " (or
                             (mode . yaml-mode)))

               ("js       " (or
                             (mode . js2-mode)
                             (mode . js3-mode)
                             (mode . javascript-mode)
                             (mode . espresso-mode)
                             (filename . "js$")))
               ("doc      " (or
                             (mode . rdoc-mode)
                             (mode . markdown-mode)
                             (mode . rst-mode)))
               ("style    " (or
                             (mode . sass-mode)
                             (mode . scss-mode)
                             (mode . css-mode)))
               ("dired   " (mode . dired-mode))
               ("shell   " (or
                            (mode . eshell-mode)
                            (mode . shell-mode)))
               ("man      " (or
                             (mode . woman-mode)
                             (mode . man-mode)))
               ("emacs   " (or
                            (mode . emacs-lisp-mode)
                            (name . "^\\*scratch\\*$")
                            (name . "^\\*Messages\\*$"))))
              )))


(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))


;; Override ibuffer function to use ido
(defun ibuffer-find-file (file &optional wildcards)
  "Like `find-file', but default to the directory of the buffer at point."
  (interactive
   (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                              (if (buffer-live-p buf)
                                  (with-current-buffer buf
                                    default-directory)
                                default-directory))))
     (list (ido-read-file-name "Find file: " default-directory)
           t)))
  (find-file file wildcards))
