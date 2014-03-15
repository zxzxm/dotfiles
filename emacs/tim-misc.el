;; -*- mode: Emacs-Lisp; fill-column: 75; comment-column: 50; -*-
;;----------------------------------------------------------------
;; Miscellaneous stuff
;;----------------------------------------------------------------
;; Last modified: <2014-01-28 08:54:22 thermans>
;;----------------------------------------------------------------

;;...........................................................
;; Misc

(ido-vertical-mode t)

(require 'nav)
(nav-disable-overeager-window-splitting)

;; Time stamps
(setq time-stamp-pattern nil)
(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-pattern "8/Last modified:[ \t]+\\\\?[\"<]+%:y-%02m-%02d %02H:%02M:%02S %u\\\\?[\">]")

;; Auto-complete with dabbrev
(add-to-list 'ac-sources 'ac-source-dabbrev)
(defun ac-dabbrev-expand ()
   (interactive)
   (auto-complete '(ac-source-dabbrev)))

(global-set-key "\M-/" 'ac-dabbrev-expand)

;;...........................................................
;; Confluence
(setq confluence-auto-save-dir prelude-cache-dir)

(setq confluence-url "https://wiki.io.comcast.net/rpc/xmlrpc")
(setq confluence-default-space-alist (quote (("https://wiki.io.comcast.net/display/VMSS" . "VMSS"))))

(setq confluence-save-credentials t)
(setq confluence-save-page-minor-edits t)

(setq confluence-default-space-alist
  (quote
   (("https://wiki.io.comcast.net/display/MPS" . "MPS"))))

(setq confluence-save-credentials t)
(setq confluence-save-page-minor-edits t)


;;...........................................................
;; Shell-FM
(add-to-list 'load-path (concat prelude-vendor-dir "/shellfm"))
(autoload 'shellfm "Shell-FM" nil t)

;;...........................................................
;; Pianobar
(add-to-list 'load-path (concat prelude-vendor-dir "/pianobar"))
(autoload 'pianobar "pianobar" nil t)

;;...........................................................
;; EMMS

;; (require 'emms-lastfm-client)
;; (setq emms-lastfm-client-api-key "ecb7f928850a3da75715d7b0ed381eed")
;; (setq emms-lastfm-client-api-secret-key "f7ae5e7bcae844f185c2aad54ff4f260")

;; (setq emms-lastfm-client-username "nedludd")
;; (setq emms-lastfm-password "f00bar")

;;...........................................................
;; MediaWiki
(require 'mediawiki)
(setq mediawiki-site-alist (quote (("WikEmacs" "http://wikemacs.org/w/" "nedludd" "f00bar" "Main Page") ("Wikipedia" "https://en.wikipedia.org/w" "thermans" "f00bar" "Main Page"))))
(setq mediawiki-site-default "WikEmacs")
(setq mediawiki-mode-hook (lambda ()
                            (visual-line-mode 1)))

;;...........................................................
;; RPM Spec
(autoload 'rpm-spec-mode "rpm-spec-mode.el" "RPM spec mode." t)
(setq auto-mode-alist (append '(("\\.spec" . rpm-spec-mode))
                              auto-mode-alist))

;;...........................................................
;; Sudoers
(add-to-list 'auto-mode-alist '("sudoers*" . etc-sudoers-generic-mode))

;;...........................................................
;; Copy line and comment (https://gist.github.com/prakashk/5319782)
(defun copy-comment-paste ()
  "copy active region/current line, comment, and then paste"
  (interactive)
  (unless (use-region-p)
    (progn
      (beginning-of-line 2)
      (push-mark (line-beginning-position 0))))
  (kill-ring-save (region-beginning) (region-end))
  (comment-region (region-beginning) (region-end))
  (yank)
  (exchange-point-and-mark)
  (indent-according-to-mode))

;;...........................................................
;; Parse ANSI codes in text and log files
;(require 'tty-format)
;(add-hook 'find-file-hooks 'tty-format-guess)


;;...........................................................
;; File Bookmarks
(require 'bookmark+)
(setq bmkp-bmenu-commands-file (concat prelude-savefile-dir "/bookmark-plus-commands.el"))
(setq bmkp-bmenu-state-file (concat prelude-savefile-dir "/bookmark-plus-state.el"))

;; (autoload 'bookmark-completing-read "bookmark" "File Bookmarks" nil t)

;; (setq bookmark-version-control (quote never))
;; (setq bookmark-save-flag 1)   ; Save after every add or delete
;; (setq bookmark-sort-flag t)   ; Sort by LIFO


;;...........................................................
;; Pretty Lambdas
(require 'pretty-lambdada)
(pretty-lambda-for-modes)

;; Diminish  ............................................... &diminish ...
;; (require 'diminish)
;; (diminish 'undo-tree-mode)
;; (diminish 'prelude-mode)
;; (diminish 'projectile-mode)
;; (diminish 'guru-mode)
;; (diminish 'eldoc-mode)
;; (diminish 'volatile-highlights-mode)
;; (diminish 'ruby-block-mode)
;; (diminish 'paredit-mode)
;; (diminish 'auto-complete-mode)
;; (diminish 'rainbow-mode)

;; Turn off a gnutls error
;; "gnutls.c: [1] Note that the security level of the Diffie-Hellman key exchange
;; has been lowered to 256 bits and this may allow decryption of the session data

(setq gnutls-min-prime-bits 1024)

;; TinyBookmark .......................................... &tinybookmark ...

(message "Loading tiny:tinybookmark...")
(require 'tinybookmark)

(setq tinybookmark-:max-col (quote (progn (tinybookmark-calc-max-col))))

;; Keys
(global-set-key [C-down-mouse-1]         'tinybookmark-mouse)
(global-set-key [(control shift button1)]  'tinybookmark-mouse-parse)


(global-unset-key (kbd "C-c t")) ;; Get rid of the prelude binding
(ti::use-prefix-key global-map "\C-c")
(global-set-key (kbd "C-c ts")            'tinybookmark-keyboard)
(global-set-key (kbd "C-c tp")            'tinybookmark-keyboard-parse)
(global-set-key (kbd "C-c b")            'tinybookmark-insert)

;;(global-set-key (kbd "S-left")  'tinybookmark-backward)
;;(global-set-key [(shift right)] 'tinybookmark-forward)
