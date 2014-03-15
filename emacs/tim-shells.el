;; -*- mode: Emacs-Lisp; fill-column: 75; comment-column: 50; -*-
;;---------------------------------------------------------------
;; Shells
;;---------------------------------------------------------------
; Last Saved Last modified: <2012-12-20 16:05:42 (thermans)>
;;---------------------------------------------------------------

;;{{{ Eshell .............................................. &eshell ...

;;(load "em-joc")

(setq eshell-directory-name (concat prelude-savefile-dir "/eshell"))

(add-hook 'eshell-post-command-hook 'eshell-show-maximum-output)
;;(setq eshell-prompt-function (quote joc-eshell-prompt))

;; Ignore duplicates in history
(setq eshell-hist-ignoredups t)

;; Just save the history file automatically
(setq eshell-ask-to-save-history nil)
(setq eshell-save-history-on-exit t)

;; Include Lisp completion in TAB completions
(setq eshell-show-lisp-completions t)

;; Don't add anything to the end of a completion
(setq comint-completion-addsuffix nil)

;; Go home when you "cd" without args
(setq eshell-pushd-tohome t)

;; Only add unique dirs to the stack
(setq eshell-pushd-dunique t)


;; Go to the bottom of the screen always.
(setq eshell-scroll-to-bottom-on-input (quote all))
(setq eshell-scroll-to-bottom-on-output (quote all))
(setq comint-scroll-to-bottom-on-output t)
(setq comint-scroll-to-bottom-on-input t)

;; Extra modules to use
(setq eshell-modules-list (quote
                           (eshell-alias
                            eshell-banner
                            eshell-basic
                            eshell-cmpl
                            eshell-dirs
                            eshell-glob
                            eshell-hist
                            eshell-ls
                            eshell-pred
                            eshell-prompt
                            eshell-rebind
                            eshell-script
                            eshell-term
                            eshell-unix)))

;; Eshell Functions ---------------------------------------------

;; ;; Run perldoc
(defun eshell/perldoc (&rest args)
   "Like `eshell/man', but invoke `perldoc'."
   (funcall 'perldoc (apply 'eshell-flatten-and-stringify args)))


;; ;; Make 'vi' open a file in emacs
(defun eshell/vi (&rest args)
   "Invoke `find-file' on the file.
   \"vi +42 bar\" also goes to line 42 in the buffer."
     (while args
       (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
         (let* ((line (string-to-number (match-string 1 (pop args))))
                (file (pop args)))
           (find-file file)
           (goto-line line))
         (find-file (pop args)))))

;; Don't start a new emacs inside eshell
(defun eshell/emacs (&rest args)
      "Open a file in emacs. Some habits die hard."
      (if (null args)
          ;; If I just ran "emacs", I probably expect to be launching
          ;; Emacs, which is rather silly since I'm already in Emacs.
          ;; So just pretend to do what I ask.
          (bury-buffer)
        ;; We have to expand the file names or else naming a directory in an
        ;; argument causes later arguments to be looked for in that directory,
        ;; not the starting directory
        (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

;; ;; Run Info
(defun eshell/info (subject)
    "Read the Info manual on SUBJECT."
    (let ((buf (current-buffer)))
      (Info-directory)
      (let ((node-exists (ignore-errors (Info-menu subject))))
        (if node-exists
            0
          ;; We want to switch back to *eshell* if the requested
          ;; Info manual doesn't exist.
          (switch-to-buffer buf)
          (eshell-print (format "There is no Info manual on %s.\n"
                        subject))
          1))))

(defun tlh-eshell-view-file (file)
    "A version of `view-file' which properly respects the eshell prompt."
    (interactive "fView file: ")
    (unless (file-exists-p file) (error "%s does not exist" file))
    (let ((had-a-buf (get-file-buffer file))
          (buffer (find-file-noselect file)))
      (if (eq (with-current-buffer buffer (get major-mode 'mode-class))
              'special)
          (progn
            (switch-to-buffer buffer)
            (message "Not using View mode because the major mode is special"))
        (let ((undo-window (list (window-buffer) (window-start)
                                 (+ (window-point)
                                    (length (funcall eshell-prompt-function))))))
          (switch-to-buffer buffer)
          (view-mode-enter (cons (selected-window) (cons nil undo-window))
                           'kill-buffer)))))

   (defun eshell/less (&rest args)
    "Invoke `view-file' on a file. \"less +42 bar\" will go to line 42 in
    the buffer for bar."
    (while args
      (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
          (let* ((line (string-to-number (match-string 1 (pop args))))
                 (file (pop args)))
            (tlh-eshell-view-file file)
            (goto-line line))
        (tlh-eshell-view-file (pop args)))))

  (defalias 'eshell/more 'eshell/less)


;; Make ls clickable and mouse aware.
(eval-after-load "em-ls"
  '(progn
     (defun ted-eshell-ls-find-file-at-point (point)
       "RET on Eshell's `ls' output to open files."
       (interactive "d")
       (find-file (buffer-substring-no-properties
                   (previous-single-property-change point 'help-echo)
                   (next-single-property-change point 'help-echo))))

     (defun pat-eshell-ls-find-file-at-mouse-click (event)
       "Middle click on Eshell's `ls' output to open files.
        From Patrick Anderson via the wiki."
       (interactive "e")
       (ted-eshell-ls-find-file-at-point (posn-point (event-end event))))

     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "RET")      'ted-eshell-ls-find-file-at-point)
       (define-key map (kbd "<return>") 'ted-eshell-ls-find-file-at-point)
       (define-key map (kbd "<mouse-1>") 'pat-eshell-ls-find-file-at-mouse-click)
       (defvar ted-eshell-ls-keymap map))

     (defadvice eshell-ls-decorated-name (after ted-electrify-ls activate)
       "Eshell's `ls' now lets you click or RET on file names to open them."
       (add-text-properties 0 (length ad-return-value)
                            (list 'help-echo "RET, mouse-2: visit this file"
                                  'mouse-face 'highlight
                                  'keymap ted-eshell-ls-keymap)
                            ad-return-value)
       ad-return-value)))

;; Bookmark integration
(defun pcomplete/eshell-mode/bmk ()
  "Completion for `bmk'"
  (pcomplete-here (bookmark-all-names)))

(defun eshell/bmk (&rest args)
  "Integration between EShell and bookmarks.
For usage, execute without arguments."
  (setq args (eshell-flatten-list args))
  (let ((bookmark (car args))
        filename name)
    (cond
     ((eq nil args)
      (format "Usage: bmk BOOKMARK to change directory pointed to by BOOKMARK
    or bmk . BOOKMARK to bookmark current directory in BOOKMARK.
Completion is available."))
     ((string= "." bookmark)
      ;; Store current path in EShell as a bookmark
      (if (setq name (car (cdr args)))
          (progn
            (bookmark-set name)
            (bookmark-set-filename name (eshell/pwd))
            (format "Saved current directory in bookmark %s" name))
        (error "You must enter a bookmark name")))
     (t
       ;; Assume the user wants to go to the path pointed out by a bookmark.
       (if (setq filename (cdr (car (bookmark-get-bookmark-record bookmark))))
           (if (file-directory-p filename)
               (eshell/cd filename)
             ;; TODO: Handle this better and offer to go to directory
             ;; where the file is located.
             (error "Bookmark %s points to %s which is not a directory"
                    bookmark filename))
         (error "%s is not a bookmark" bookmark))))))

;;}}

;;{{{ Multi Term ......................................... &multi-term ...

(autoload 'multi-term "multi-term" nil t)
(autoload 'multi-term-next "multi-term" nil t)

;; (setq multi-term-program "/bin/zsh")

(add-hook 'term-mode-hook
 ;; #'(lambda () (setq autopair-dont-activate t))
  (lambda ()
    (define-key term-mode-map (kbd "C-c t") 'multi-term-next)))

;; (global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term) ;; create a new terminal


;;}}


;;{{{ Bash Shell ............................................. &bash ...

(add-hook 'comint-output-filter-functions
   'shell-strip-ctrl-m nil t)
(add-hook 'comint-output-filter-functions
    'comint-watch-for-password-prompt nil t)
(setq explicit-shell-file-name "bash")

;; For subprocesses invoked via the shell
;; (e.g., "shell -c command")
;;(setq explicit-shell-file-name shell-file-name)

;;(setq shell-command-switch "-c")
(setenv "SHELL" shell-file-name)
(setenv "TERM" "emacs")
(setq explicit-bash-args '("-login" "-i"))
(if (boundp 'w32-quote-process-args)
    (setq w32-quote-process-args ?\"))

;(add-to-list 'process-coding-system-alist
;              '("bash" . (undecided-dos . undecided-unix)))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(add-hook 'shell-mode-hook
          '(lambda ()
             (local-set-key [home]        ; move to beginning of line, after prompt
                            'comint-bol)
             (local-set-key [up]          ; cycle backward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-previous-input 1)
                                 (previous-line 1))))
             (local-set-key "\C-p"        ; cycle backward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-previous-input 1)
                                 (previous-line 1))))
             (local-set-key [down]        ; cycle forward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-next-input 1)
                                 (forward-line 1))))
             (local-set-key "\C-n"        ; cycle forward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-next-input 1)
                                 (forward-line 1))))
             (local-set-key "\C-u" 'comint-kill-input) ; wipe out the command line
             (line-number-mode nil)
             (column-number-mode nil)
             ))



;;}}}
