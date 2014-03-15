;; -*- mode: Emacs-Lisp -*-
;;---------------------------------------------------------------
;; Core Stuff
;;---------------------------------------------------------------
;; Last modified: <2013-07-15 14:16:04 (thermans)>
;;---------------------------------------------------------------

;; Disable the startup splash screen
(setq inhibit-startup-message t)

;; Automatically indent
(global-set-key (kbd "RET") 'newline-and-indent)

(setq gc-cons-threshold 50000000)
(require 'cl)

(setq cursor-color "orange")

;; Use Cua only for rectangles
(setq cua-enable-cua-keys nil)
(cua-mode t)

;; Set mark
(global-set-key (kbd "C-c .") 'cua-set-mark)

;; Disable pesky modes
(setq prelude-whitespace nil)
(setq prelude-flyspell nil)

;; Load the theme directory
(add-to-list 'load-path (concat prelude-dir "themes/"))
;; (load-theme 'railscasts t)

;; Scratch Buffer .................................... &scratch ...
;; Recreate the scratch buffer if it gets killed
(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))

(defun kill-scratch-buffer ()
  ;; The next line is just in case someone calls this manually
  (set-buffer (get-buffer-create "*scratch*"))
  ;; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))
  ;; Make a brand new *scratch* buffer
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  ;; Since we killed it, don't let caller do that.
  nil)

;; Go to the scratch buffer and back
;; (load-file (concat my-package-dir "scrat.el" ))
;;(require 'scrat)
;;(global-set-key (kbd "C-c r") 'back-from-scratch)

(setq initial-scratch-message
      ";; Hey Tim!!!

"
      )

;; Mode aware scratch buffers
(autoload 'scratch "scratch" nil t)
(global-set-key (kbd "C-c C-s") 'scratch)

;; Clipboard (http://www.emacswiki.org/emacs/CopyAndPaste)
(setq mouse-drag-copy-region nil)                 ; Do not use clipboard on mouse drag
(setq x-select-enable-primary nil)                ; Do not yank to primary
(setq x-select-enable-clipboard t)

(setq interprogram-paste-function 'x-select-text)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(setq select-active-regions t)
(global-set-key [mouse-2] 'mouse-yank-primary)

(setq yank-pop-change-selection t)

(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)


;; Mode sensitive comments
;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(global-set-key "\M-;" 'comment-dwim-line)
;; (global-set-key (kbd "C-c c") 'comment-dwim-line)

;;...........................................................
;; Kill emacs completely
(global-set-key (kbd "C-x M-c") 'save-buffers-kill-emacs)

;;...........................................................
;; Increase these values for debug and jde, etc.
(setq max-lisp-eval-depth 22000)
(setq max-specpdl-size 100000)

;; Scratch Buffer .................................... &scratch ...
;; Recreate the scratch buffer if it gets killed
(save-excursion
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))

(defun kill-scratch-buffer ()
  ;; The next line is just in case someone calls this manually
  (set-buffer (get-buffer-create "*scratch*"))
  ;; Kill the current (*scratch*) buffer
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))
  ;; Make a brand new *scratch* buffer
  (set-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  ;; Since we killed it, don't let caller do that.
  nil)

(setq initial-scratch-message
      ";; Hey Tim!!!

"
      )

;; ..........................................................
;;   Line numbers on
(line-number-mode t)

;; ..........................................................
;; Better undo mode
(global-undo-tree-mode)

;; ...........................................................
;; Cut ^M's
(global-set-key [f7] 'cut-ctrlM) ; cut all ^M.

(defun cut-ctrlM ()
  "Cut all visible ^M."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match "")))
  t)

;;...........................................................
;; Ace Jump mode
(global-set-key (kbd "C-0") 'ace-jump-mode)

;;...........................................................
;;  Find the last change you made
(autoload 'goto-last-change "goto-last-change"
  "Set point to the position of the last change." t)
(global-set-key (kbd "C-x C-\\") 'goto-last-change)

;;...........................................................
;; Browse Kill ring
(require 'browse-kill-ring)
(global-set-key (kbd "C-c k") 'browse-kill-ring)
(browse-kill-ring-default-keybindings)


;;...........................................................
;; Turn off the scroll bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;...........................................................
;; Set the tmp directory
(setq temporary-file-directory "/tmp/")


;; Woman  ................................................... &woman ...
;; Use WOman for everything
(autoload 'woman "woman" "Decode and browse a UN*X man page." t)
(autoload 'woman-find-file "woman"  "Find, decode & browse a  man-page." t)
(global-set-key [f6] 'woman)

(autoload 'woman-dired-find-file "woman"
  "In dired, run the WoMan man-page browser on this file." t)

(add-hook 'dired-mode-hook
          (function
           (lambda ()
             (define-key dired-mode-map "W" 'woman-dired-find-file))))

(setq woman-cache-filename (concat prelude-savefile-dir "/woman-cache.el"))
(setq woman-cache-level 3)
(setq woman-imenu t)
(setq woman-imenu-title "Contents")
(setq woman-manpath (quote ("/usr/share/man" "/usr/local/share/man" "/home/thermans/perl5/man" )))
(setq woman-use-symbol-font t)
(setq woman-use-topic-at-point t)
(setq woman-use-topic-at-point-default t)

;;Use color instead of underscores
(defvar woman-always-colour-faces)
(set-default 'woman-always-colour-faces 't)

;;Fill up the full width of the frame
(defvar woman-fill-frame)
(set-default 'woman-fill-frame 't)

;;; .................................................. &smart-modeline ...
;;(setq sml/theme 'dark)
;;(require 'smart-mode-line)
;;(sml/setup)

;;; ....................................................... &powerline ...
;; Pretty mode line
;;(powerline-default-center)
(powerline-default-theme)

;; Backups  ............................................... &backups ...

(setq-default make-backup-files t)
(setq backup-directory-alist
            `((".*" . ,prelude-backup-dir)))

(setq auto-save-file-name-transforms
      `((".*" ,(concat prelude-personal-dir "/cache/auto-saves/"))))

(setq auto-save-list-file-prefix "~/.emacs.d/personal/cache/auto-saves/saves-")

;; Goto line
(global-set-key (kbd "M-g") 'goto-line)


;; Smex - ido for commands ................................ &smex ...
(setq smex-save-file (concat prelude-savefile-dir "/smex.save"))
;;(smex-auto-update)

(global-set-key "\M-x" 'smex)
(global-set-key "\M-X" 'smex-major-mode-commands)
;; (global-set-key "\C-c \M-x" 'smex-update-and-run)
;; This is your old M-x.
;; (global-set-key "\C-c \C-c \M-x" 'execute-extended-command)

(add-hook 'after-init-hook 'smex-initialize)
