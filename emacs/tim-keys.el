;; -*- mode: Emacs-Lisp; fill-column: 75; comment-column: 50; -*-
;;----------------------------------------------------------------
;; Keys
;;----------------------------------------------------------------
;; Last modified: <2013-10-29 11:15:48 (thermans)>
;;----------------------------------------------------------------

;; (require 'key-chord)
(key-chord-mode -1)  ; turning it off.  Interferes with evil
;; ;; and some chords, for example
;; (key-chord-define-global "xx"     'smex)
;; (key-chord-define-global "vv"     'cua-scroll-down)
;; (key-chord-define-global "ff"     'cua-scroll-up)

(global-set-key (kbd "C-x f") 'prelude-recentf-ido-find-file)

;; Swap these twos
(global-set-key (kbd "M-%")   'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)

;; Evaluate the current buffer
(global-set-key (kbd "C-c v") 'eval-buffer)

;; Revert buffer
(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t nil)))

;; Kill the buffer
(global-set-key [f4]		'kill-this-buffer)

;; Browse URL
;;(global-set-key (kbd "C-c b") 'browse-url-at-point)

;; Locate and Load a library
;;(global-set-key (kbd "C-c l") 'ilocate-load-library)

;; .............................................. &multiple-cursors ...
;; Expand region (increases selected region by semantic units)
(global-set-key (kbd "C-;") 'er/expand-region)

(global-unset-key (kbd "M-c")) ;; unset "Capitalize word" binding

;; Experimental multiple-cursors
(global-set-key (kbd "M-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "M-c C-a") 'mc/edit-beginnings-of-lines)
;;; (global-set-key (kbd "M-c C-e") 'mc/edit-lines)

;; Mark additional regions matching current region
(global-set-key (kbd "M-'") 'mc/mark-all-dwim)
(global-set-key (kbd "M-=") 'mc/mark-all-in-region)

;;;(global-set-key (kbd "C-å") 'mc/mark-previous-like-this)
;;;(global-set-key (kbd "M-=") 'mc/mark-next-like-this)
;;;(global-set-key (kbd "M-=") 'mc/mark-more-like-this-extended)

;;; Symbol and word specific mark-more
;;; (global-set-key (kbd "s-æ")   'mc/mark-next-word-like-this)
;;; (global-set-key (kbd "s-å")   'mc/mark-previous-word-like-this)
;;; (global-set-key (kbd "M-s-æ") 'mc/mark-all-words-like-this)
;;; (global-set-key (kbd "s-Æ")   'mc/mark-next-symbol-like-this)
;;; (global-set-key (kbd "s-Å")   'mc/mark-previous-symbol-like-this)
;;; (global-set-key (kbd "M-s-Æ") 'mc/mark-all-symbols-like-this)

;; Extra multiple cursors stuff
(global-set-key (kbd "C-~") 'mc/reverse-regions)
(global-set-key (kbd "M-~") 'mc/sort-regions)
(global-set-key (kbd "M-~") 'mc/insert-numbers)

(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; Set anchor to start rectangular-region-mode
(global-set-key (kbd "C-,") 'set-rectangular-region-anchor)
