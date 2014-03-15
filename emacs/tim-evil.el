;; -*- mode: Emacs-Lisp; fill-column: 75; comment-column: 50; -*-
;;---------------------------------------------------------------
;; Evil configuration
;;---------------------------------------------------------------
;; Last modified: <2014-01-04 13:00:54 therma000>
;;----------------------------------------------------------------

;; Evil leader
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "1" 'delete-other-windows
  "b" 'ido-switch-buffer
  "e" 'nav-toggle
  "f" 'ido-find-file
  "F" 'prelude-recentf-ido-find-file
  "k" 'kill-this-buffer
  "w" 'save-buffer
  "v" 'describe-variable
  "x" 'smex
  )

;; Start Evil
(evil-mode 1)

;; Nerd Commenter -----------------------------------------------
;; These are the bindings:
;; (global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
;; (global-set-key (kbd "C-c l") 'evilnc-comment-or-uncomment-to-the-line)
;; (global-set-key (kbd "C-c c") 'evilnc-copy-and-comment-lines)
;; (global-set-key (kbd "C-c p") 'evilnc-comment-or-uncomment-paragraphs)
;; (eval-after-load 'evil
;;      (define-key evil-normal-state-map ",ci" 'evilnc-comment-or-uncomment-lines)
;;      (define-key evil-normal-state-map ",cl" 'evilnc-comment-or-uncomment-to-the-line)
;;      (define-key evil-normal-state-map ",cc" 'evilnc-copy-and-comment-lines)
;;      (define-key evil-normal-state-map ",cp" 'evilnc-comment-or-uncomment-paragraphs)
;;      (define-key evil-normal-state-map ",cr" 'comment-or-uncomment-region)
(evilnc-default-hotkeys)


;; change mode-line color by evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
            (lambda ()
              (let ((color (cond ((minibufferp) default-color)
                                 ((evil-insert-state-p) '("#8c5353" . "#ffffff"))
                                 ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                                 ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
                                 (t default-color))))
                (set-face-background 'mode-line (car color))
           (set-face-foreground 'mode-line (cdr color))))))

;; Evil matchit
;; (global-evil-matchit-mode 1)

;; Matchit is turned on for specific modes.  See "tim-web.el"
;; Use "turn-on-evil-matchit-mode" in mode hook
