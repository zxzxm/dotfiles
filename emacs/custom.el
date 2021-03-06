(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-start 3)
 '(ac-modes (quote (emacs-lisp-mode lisp-mode lisp-interaction-mode slime-repl-mode c-mode cc-mode c++-mode java-mode malabar-mode clojure-mode scala-mode scheme-mode ocaml-mode tuareg-mode haskell-mode perl-mode cperl-mode python-mode ruby-mode ecmascript-mode javascript-mode js-mode js2-mode php-mode css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode lua-mode haml-mode sass-mode scss-mode js3-mode conf-mode confluence-mode)))
 '(auto-save-list-file-prefix "~/.emacs.d/personal/cache/auto-saves/saves-")
 '(baud-rate 19200)
 '(blink-matching-paren nil)
 '(bmkp-bmenu-image-bookmark-icon-file nil)
 '(bmkp-bmenu-state-file "/home/thermans/.emacs.d/savefile/bookmark-plus-state.el")
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/savefile/bookmarks")
 '(bookmark-save-flag 1)
 '(bookmark-version-control (quote nospecial))
 '(byte-compile-verbose nil)
 '(column-number-mode t)
 '(comint-completion-addsuffix nil)
 '(comint-move-point-for-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(confluence-default-space-alist (quote (("https://wiki.io.comcast.net/display/MPS" . "MPS"))) t)
 '(confluence-save-credentials t t)
 '(confluence-save-page-minor-edits t t)
 '(cua-enable-cua-keys nil)
 '(cua-mode t nil (cua-base))
 '(custom-file "/home/thermans/.emacs.d/personal/custom.el")
 '(custom-safe-themes (quote ("6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "025354235e98db5e7fd9c1a74622ff53ad31b7bde537d290ff68d85665213d85" default)))
 '(custom-theme-directory "~/.emacs.d/themes/")
 '(deft-directory "/home/thermans/Dropbox/deft/")
 '(deft-extension "org")
 '(deft-use-filename-as-title t)
 '(delete-old-versions (quote other))
 '(dired-dwim-target t)
 '(dired-listing-switches "-aGghlv --group-directories-first --time-style=long-iso")
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(emms-cache-file "~/.emacs.d/emms/cache")
 '(emms-lastfm-client-api-key "ecb7f928850a3da75715d7b0ed381eed")
 '(emms-lastfm-client-api-secret-key "f7ae5e7bcae844f185c2aad54ff4f260")
 '(emms-lastfm-client-username "nedludd")
 '(erc-autoaway-idle-seconds 600)
 '(erc-interpret-mirc-color t)
 '(erc-kill-buffer-on-part t)
 '(erc-kill-queries-on-quit t)
 '(erc-kill-server-buffer-on-quit t)
 '(erc-log-channels-directory "~/Dropbox/im/erc/")
 '(erc-nick "tibaza")
 '(erc-query-display (quote buffer))
 '(erc-server-coding-system (quote (utf-8 . utf-8)))
 '(erc-spelling-mode t)
 '(erc-track-mode t)
 '(erc-truncate-mode t)
 '(evil-overriding-maps (quote ((Buffer-menu-mode-map) (color-theme-mode-map) (comint-mode-map) (compilation-mode-map) (dictionary-mode-map) (ert-results-mode-map . motion) (Info-mode-map . motion) (speedbar-key-map) (speedbar-file-key-map) (speedbar-buffers-key-map) (simple-rtm-mode-map) (simple-rtm-details-mode-map))))
 '(explicit-shell-file-name "bash")
 '(flx-ido-mode t)
 '(flycheck-checkers (quote (asciidoc c/c++-clang c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint css-csslint d-dmd elixir emacs-lisp erlang eruby-erubis go-gofmt go-golint go-vet go-build go-test haml handlebars haskell-ghc haskell-hlint html-tidy javascript-jshint javascript-eslint javascript-gjslint json-jsonlint less lua make perl perl-perlcritic php php-phpmd php-phpcs puppet-parser puppet-lint python-flake8 python-pylint racket rst rst-sphinx ruby-rubocop ruby-rubylint ruby ruby-jruby rust sass scala scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim tex-chktex tex-lacheck texinfo verilog-verilator xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby)))
 '(flycheck-disabled-checkers (quote (emacs-list-checkdoc)))
 '(frame-background-mode (quote dark))
 '(fringe-mode 4 nil (fringe))
 '(gc-cons-threshold 50000000)
 '(global-anzu-mode t)
 '(global-auto-revert-mode t)
 '(global-hl-line-mode t)
 '(global-undo-tree-mode t)
 '(helm-default-external-file-browser "pcmanfm")
 '(helm-descbinds-mode t)
 '(helm-match-plugin-mode t nil (helm-match-plugin))
 '(html-site-current "Hermans.net")
 '(html-site-list (quote (("Hermans.net" "/home/thermans/localsrc/hermans.net" "" "" "" "" "" nil "hermans.net" "hermanst@hermans.net" "" "/home/thermans/hermans.net" "" "http://www.hermans.net" "/" ""))))
 '(ido-auto-merge-work-directories-length -1)
 '(ido-create-new-buffer (quote always))
 '(ido-default-file-method (quote selected-window))
 '(ido-enable-dot-prefix t)
 '(ido-enable-flex-matching t)
 '(ido-max-dir-file-cache 10000)
 '(ido-max-prospects 10)
 '(ido-mode (quote both) nil (ido))
 '(ido-show-dot-for-dired t)
 '(ido-ubiquitous-mode t)
 '(ido-use-faces nil)
 '(ido-use-filename-at-point nil)
 '(ido-use-virtual-buffers t)
 '(imenu-auto-rescan t)
 '(indent-tabs-mode nil)
 '(inferior-js-program-command "/usr/bin/rhino")
 '(inhibit-startup-screen t)
 '(initial-scratch-message ";; Hey Tim!!!

")
 '(ispell-program-name "aspell")
 '(js3-auto-indent-p t)
 '(js3-curly-indent-offset 2)
 '(js3-expr-indent-offset 2)
 '(js3-indent-on-enter-key t)
 '(js3-indent-tabs-mode t)
 '(js3-lazy-commas t)
 '(js3-lazy-dots t)
 '(js3-lazy-operators t)
 '(js3-paren-indent-offset 2)
 '(js3-square-indent-offset 2)
 '(keyboard-coding-system (quote utf-8-unix))
 '(max-lisp-eval-depth 22000)
 '(max-specpdl-size 100000)
 '(mediawiki-site-default "WikEmacs")
 '(mml-secure-passphrase-cache-expiry 16)
 '(multi-term-program "/bin/zsh")
 '(nav-width 25)
 '(org-capture-templates (quote (("n" "Notes" entry (file "~/Dropbox/org/notes.org") "* %U
%a
%?"))) t)
 '(package-archives (quote (("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(package-user-dir "/home/thermans/.emacs.d/elpa")
 '(password-cache-expiry nil)
 '(prelude-flyspell nil)
 '(prelude-global-mode t)
 '(prelude-whitespace nil)
 '(projectile-cache-file "/home/thermans/.emacs.d/savefile/projectile.cache")
 '(projectile-enable-caching t)
 '(projectile-global-mode t)
 '(projectile-known-projects-file "/home/thermans/.emacs.d/savefile/projectile-bookmarks.eld")
 '(python-shell-interpreter "python2")
 '(rainbow-html-colors-major-mode-list (quote (html-mode css-mode php-mode nxml-mode xml-mode sass-mode scss-mode)))
 '(reb-re-syntax (quote string))
 '(recentf-max-menu-items 15)
 '(recentf-max-saved-items 500)
 '(recentf-mode t)
 '(rng-schema-locating-files (quote ("schemas.xml" "/usr/share/emacs/24.0.93/etc/schema/schemas.xml" "/home/thermans/.emacs.d/rnc/html5/html5-schemas.xml")))
 '(rsense-home "/home/thermans/.emacs.d/vendor/rsense")
 '(rst-level-face-base-color "black")
 '(safe-local-variable-values (quote ((eval when (fboundp (quote rainbow-mode)) (rainbow-mode 1)) (Coding . utf-8) (ruby-compilation-executable . "ruby") (ruby-compilation-executable . "ruby1.8") (ruby-compilation-executable . "ruby1.9") (ruby-compilation-executable . "rbx") (ruby-compilation-executable . "jruby"))))
 '(save-place t nil (saveplace))
 '(savehist-autosave-interval 60)
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 100000)
 '(scroll-preserve-screen-position 1)
 '(shell-file-name "/bin/zsh")
 '(shellfm-confirm-skip nil)
 '(shift-select-mode nil)
 '(show-paren-mode t)
 '(show-smartparens-global-mode t)
 '(size-indication-mode t)
 '(sml/theme (quote respectful))
 '(term-bind-key-alist (quote (("C-c C-c" . term-interrupt-subjob) ("C-p" . term-send-up) ("C-n" . term-send-down) ("C-s" . isearch-forward) ("C-r" . term-send-reverse-search-history) ("C-m" . term-send-raw) ("M-f" . term-send-forward-word) ("M-b" . term-send-backward-word) ("M-o" . term-send-backspace) ("M-p" . previous-line) ("M-n" . next-line) ("M-M" . term-send-forward-kill-word) ("M-N" . term-send-backward-kill-word) ("M-r" . term-send-reverse-search-history) ("M-," . term-send-input) ("M-." . comint-dynamic-complete))))
 '(tool-bar-mode nil)
 '(tramp-auto-save-directory "/home/thermans/.emacs.d/savefile/auto-saves/")
 '(tramp-default-method "ssh")
 '(tramp-default-user "therma000")
 '(tramp-default-user-alist (quote (("\\`smb\\'" nil nil) ("\\`\\(?:fcp\\|krlogin\\|r\\(?:cp\\|emcp\\|sh\\)\\|telnet\\)\\'" nil "thermans") ("\\`\\(?:ksu\\|su\\(?:do\\)?\\)\\'" nil "root") ("\\`\\(?:socks\\|tunnel\\)\\'" nil "thermans") ("\\`synce\\'" nil nil) ("ssh" ".*.glenwoodpool.net" "glenwood"))))
 '(tramp-persistency-file-name (concat prelude-savefile-dir "/tramp"))
 '(tramp-shell-prompt-pattern "\\(?:^\\|\\)[^]#$%~>
]*#?[]#$%~>] *\\(\\[[0-9;]*[a-zA-Z] *\\)*")
 '(tramp-verbose 9)
 '(tree-widget-image-enable nil)
 '(uniquify-ignore-buffers-re "^\\*")
 '(uniquify-separator "/")
 '(url-history-track t)
 '(url-personal-mail-address "tim_hermans@cable.comcast.com")
 '(vc-make-backup-files t)
 '(version-control t)
 '(volatile-highlights-mode t)
 '(which-function-mode t)
 '(winner-mode t)
 '(yank-pop-change-selection t)
 '(yas-global-mode t nil (yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3f3f3f" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "unknown" :family "Ubuntu Mono"))))
 '(bmkp-heading ((t (:foreground "#9c6363"))))
 '(bmkp-local-directory ((t (:foreground "#dfaf8f"))))
 '(bmkp-remote-file ((t (:foreground "#7f9f7f"))))
 '(confluence-panel-face ((t (:background "gray21"))) t)
 '(cperl-array-face ((t (:foreground "#8c5353" :weight bold))) t)
 '(cperl-hash-face ((t (:foreground "#5f7f5f" :weight bold))) t)
 '(helm-ff-directory ((t (:foreground "#dfaf8f"))))
 '(paren-face-match ((t (:background "#3f3f3f" :foreground "#93e0e3" :weight normal))) t)
 '(woman-bold ((t (:inherit bold :foreground "#7f9f7f"))) t)
 '(woman-unknown ((t (:foreground "#dfaf8f" :weight bold))) t))
