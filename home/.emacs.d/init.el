(setq debug-on-error t
      debug-on-quit t)

(require 'package)
(setq package-archives
      (append package-archives
              '(("melpa" . "http://melpa.org/packages/"))
              '(("org" . "http://orgmode.org/elpa/"))))
(package-initialize)
(setq package-enable-at-startup nil)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(prefer-coding-system 'utf-8)
;; (setq browse-url-browser-function 'browse-url-generic
;; browse-url-generic-program "chrome")
(setq browse-url-can-use-xdg-open t)

(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
(setq recentf-max-menu-items 100)

(setq url-proxy-services nil)
;; (setq url-proxy-services '(("http" . "192.168.8.1:3129")))

;; Core ------------------------------------------------------------------------

(require 'cl)

(use-package engine-mode
  :commands (defengine spacemacs/search-engine-select)
  :defines search-engine-alist
  :init
  (setq search-engine/init-engine-mode 'browse-url-generic)
  (evil-leader/set-key
    "a/" 'spacemacs/search-engine-select)
  (setq search-engine-alist
        '((google
           :name "Google"
           :url "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")
          (github
           :name "Github"
           :url "https://github.com/search?ref=simplesearch&q=%s")
          (stack-overflow
           :name "Stack Overflow"
           :url "https://stackoverflow.com/search?q=%s"
           )
          (wikipedia
           :name "Wikipedia"
           :url "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")))
  :config
  (engine-mode t)
  (mapcar (lambda (engine)
            (let* ((cur-engine (car engine))
                   (engine-url (plist-get (cdr engine) :url)))
              (eval `(defengine ,cur-engine ,engine-url))))
          search-engine-alist)
  (defun spacemacs//search-engine-source (engines)
    "return a source for helm selection"
    `((name . "Search Engines")
      (candidates . ,(mapcar (lambda (engine)
                               (cons (plist-get (cdr engine) :name)
                                     (intern (format "engine/search-%S"
                                                     (car engine)))))
                             engines))
      (action . (lambda (candidate) (call-interactively candidate)))))
  (defun spacemacs/search-engine-select ()
    "set search engine to use"
    (interactive)
    (helm :sources (list (spacemacs//search-engine-source
                          search-engine-alist))))
  )

(use-package evil
  :init
  (evil-mode 1)
  (setq evil-emacs-state-cursor '("red" box)
        evil-normal-state-cursor '("green" box)
        evil-visual-state-cursor '("orange" box)
        evil-insert-state-cursor '("red" bar)
        evil-replace-state-cursor '("red" bar)
        evil-operator-state-cursor '("red" hollow))
  (loop for (mode . state) in '((inferior-emacs-lisp-mode . emacs)
                                (nrepl-mode . insert)
                                (cider-repl-mode . emacs)
                                (cider-stacktrace-mode . emacs)
                                (cider-test-report-mode . emacs)
                                (pylookup-mode . emacs)
                                (comint-mode . emacs)
                                (shell-mode . insert)
                                (git-commit-mode . insert)
                                (git-rebase-mode . emacs)
                                (term-mode . emacs)
                                (help-mode . emacs)
                                (helm-grep-mode . emacs)
                                (grep-mode . emacs)
                                (bc-menu-mode . emacs)
                                (magit-branch-manager-mode . emacs)
                                (makey-key-mode . emacs)
                                (neotree-mode . emacs)
                                (rdictcc-buffer-mode . emacs)
                                (dired-mode . emacs)
                                (term-mode . emacs)
                                )
        do (evil-set-initial-state mode state))
  
  ;; use normal Emacs bindings in insert mode
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map (read-kbd-macro evil-toggle-key) 'evil-normal-state)
  (evil-global-set-key 'insert [escape] 'evil-normal-state)
  (evil-global-set-key 'insert "\C-w" 'evil-delete-backward-word)

  (define-key evil-normal-state-map "\M-." nil)
  )

(use-package evil-leader
             :init
             (setq evil-leader/leader "SPC")
             (global-evil-leader-mode 1)
             )

(use-package expand-region
             :defer t
             :init
             (evil-leader/set-key "v" 'er/expand-region)
             (setq expand-region-contract-fast-key "V"
                   expand-region-reset-fast-key "r")
             (defadvice er/prepare-for-more-expansions-internal
                 (around helm-ag/prepare-for-more-expansions-internal activate)
               ad-do-it
               (let ((new-msg (concat (car ad-return-value)
                                      ", / to search in project, "
                                      "f to search in files, "
                                      "b to search in opened buffers"))
                     (new-bindings (cdr ad-return-value)))
                 (cl-pushnew
                  '("/" (lambda ()
                          (call-interactively
                           'spacemacs/helm-project-smart-do-search-region-or-symbol)))
                  new-bindings)
                 (cl-pushnew
                  '("f" (lambda ()
                          (call-interactively
                           'spacemacs/helm-files-smart-do-search-region-or-symbol)))
                  new-bindings)
                 (cl-pushnew
                  '("b" (lambda ()
                          (call-interactively
                           'spacemacs/helm-buffers-smart-do-search-region-or-symbol)))
                  new-bindings)
                 (setq ad-return-value (cons new-msg new-bindings))))
             )

(use-package helm
  :init
  (require 'helm-config)
  (require 'helm-files)
  (require 'helm-net)
  (setq
   helm-idle-delay                        0.0
   helm-quick-update                      t
   helm-split-window-in-side-p            t ; open helm buffer inside current window, not occupy whole other window
   helm-move-to-line-cycle-in-source      t ; move to end or beginning of source when reaching top or bottom of source.
   helm-ff-skip-boring-files              t
   helm-ff-search-library-in-sexp         t ; search for library in `require' and `declare-function' sexp.
   helm-scroll-amount                     8 ; scroll 8 lines other window using M-<next>/M-<prior>
   helm-ff-file-name-history-use-recentf  t
   helm-ff-transformer-show-only-basename nil
   helm-adaptive-history-file             (concat user-emacs-directory "helm-history"))

  (helm-adaptive-mode t)                  ; use adaptive mode to rank common items
  (helm-mode 1)
  (global-unset-key (kbd "C-x c"))
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "M-x")		'helm-M-x)
  (when (executable-find "curl")
    (setq helm-net-prefer-curl t))
  )

;; Edit ------------------------------------------------------------------------

;; use only spaces and no tabs
(setq-default indent-tabs-mode nil
              tab-width 2
              ;; also fill paragraphs to 80 characters
              fill-column 80
              whitespace-line-column 80)
(setq delete-by-moving-to-trash t)
(setq sentence-end-double-space nil)    ; single space ends a sentence
(setq x-underline-at-descent-line t)
;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)
(setq set-mark-command-repeat-pop t)

;; Navigation ------------------------------------------------------------------

(global-auto-revert-mode 1)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
;; no beep pleeeeeease ! (and no visual blinking too please)
(setq ring-bell-function 'ignore
      visible-bell nil)
(xterm-mouse-mode 1)

;; Session --------------------------------------------------------------------

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
;; scratch buffer empty
(setq initial-scratch-message nil
      inhibit-startup-screen t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; seems pointless to warn. There's always undo.
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets
      uniquify-ignore-buffers-re "^\\*")

;; UI --------------------------------------------------------------------------

(setq column-number-mode t)
(global-hl-line-mode 1)
(blink-cursor-mode -1)
(tooltip-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(xterm-mouse-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
;; important for golden-ratio to better work
(setq window-combination-resize t)
;; don't let the cursor go into minibuffer prompt
;; Tip taken from Xah Lee: http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties
      '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
;; instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.02)

(diminish 'auto-fill-function " F")

(set-face-attribute 'default nil
                    :family "Fantasque Sans Mono"
                    :height 180)
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                    (font-spec :family "Source Han Sans TC Normal" :size 20)
                    )
  )

(use-package moe-theme)
(require 'moe-dark-theme)

(use-package smart-mode-line
             :init
             (setq sml/theme 'dark
                   sml/no-confirm-load-theme t)
             (sml/setup)
             )

;; Misc ------------------------------------------------------------------------

(prefer-coding-system 'utf-8)
(setq delete-by-moving-to-trash t)
(winner-mode 1)

(setq hippie-expand-try-functions-list
      '(
        yas-hippie-try-expand
        try-expand-dabbrev-visible         ; visible window
        try-complete-lisp-symbol-partially ; as many as unique
        try-complete-lisp-symbol
        try-complete-file-name-partially   ; as many as unique
        try-complete-file-name
        (lambda (arg) (call-interactively 'company-complete))
        ))

;; Packages --------------------------------------------------------------------

(use-package aggressive-indent :defer t)

(use-package avy
  :defer t
  :init
  (setq avy-all-windows 'all-frames)
  (evil-leader/set-key
                                        ; "SPC" 'avy-goto-word-or-subword-1
    "l" 'avy-goto-line)
  (define-key evil-normal-state-map "s" 'avy-goto-word-or-subword-1)
  )

(use-package company
  :init
  (setq company-tooltip-limit 20
        company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-frontends '(company-pseudo-tooltip-frontend))
  (global-company-mode 1)
  (global-set-key (kbd "C-M-i") 'company-complete)
  (setq company-backends (set-difference company-backends '(company-semantic company-clang)))
  )

(use-package company-quickhelp
             :defer t
             :config
             (add-hook 'company-mode-hook 'company-quickhelp-mode))

(use-package diff-hl
  :init
  (global-diff-hl-mode 1)
  (evil-leader/set-key
    "ghg" 'diff-hl-diff-goto-hunk
    "ghn" 'diff-hl-next-hunk
    "ghp" 'diff-hl-previous-hunk
    "ghr" 'diff-hl-revert-hunk))

(use-package edit-server
             :if window-system
             :init
             (add-hook 'after-init-hook 'server-start t)
             (add-hook 'after-init-hook 'edit-server-start t))

(use-package evil-search-highlight-persist
             :defer t
             :config
             (global-evil-search-highlight-persist 1))

(use-package evil-surround :init (global-evil-surround-mode 1))

(use-package fic-mode :defer t :init (add-hook 'prog-mode-hook 'fic-mode))

(use-package flycheck
             :init
             (global-flycheck-mode 1)
             (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck)
             )

(use-package pcre2el)
(use-package helm-ag
  :defer t
  :init
  (defun spacemacs//helm-do-ag-region-or-symbol (func &optional dir)
    "Search with `ag' with a default input."
    (require 'helm-ag)
    (cl-letf* (((symbol-value 'helm-ag-insert-at-point) 'symbol)
               ;; make thing-at-point choosing the active region first
               ((symbol-function 'this-fn) (symbol-function 'thing-at-point))
               ((symbol-function 'thing-at-point)
                (lambda (thing)
                  (let ((res (if (region-active-p)
                                 (buffer-substring-no-properties
                                  (region-beginning) (region-end))
                               (this-fn thing))))
                    (when res (rxt-quote-pcre res))))))
      (funcall func dir)))
  (defun spacemacs/helm-buffers-do-ag (&optional _)
    "Wrapper to execute `helm-ag-buffers.'"
    (interactive)
    (helm-do-ag-buffers))
  (defun spacemacs/helm-buffers-do-ag-region-or-symbol ()
    "Search in opened buffers with `ag' with a default input."
    (interactive)
    (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-buffers-do-ag))
  (defun spacemacs/helm-files-do-ag (&optional dir)
    "Search in files with `ag' using a default input."
    (interactive)
    (helm-do-ag dir))
  (defun spacemacs/helm-files-do-ag-region-or-symbol ()
    "Search in files with `ag' using a default input."
    (interactive)
    (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-ag))
  (defun spacemacs/helm-project-do-ag ()
    "Search in current project with `ag'."
    (interactive)
    (let ((dir (projectile-project-root)))
      (if dir
          (helm-do-ag dir)
        (message "error: Not in a project."))))
  (defun spacemacs/helm-project-do-ag-region-or-symbol ()
    "Search in current project with `ag' using a default input."
    (interactive)
    (let ((dir (projectile-project-root)))
      (if dir
          (spacemacs//helm-do-ag-region-or-symbol 'helm-do-ag dir)
        (message "error: Not in a project."))))

  (evil-leader/set-key
    "sb" 'spacemacs/helm-buffers-do-ag
    "sB" 'spacemacs/helm-buffers-do-ag-region-or-symbol
    "sf" 'spacemacs/helm-files-do-ag
    "sF" 'spacemacs/helm-files-do-ag-region-or-symbol
    "sp" 'spacemacs/helm-project-do-ag
    "sP" 'spacemacs/helm-project-do-ag-region-or-symbol
    )
  )

(use-package helm-descbinds
  :init (helm-descbinds-mode 1))

(use-package helm-make
  :defer t
  :init
  (evil-leader/set-key
    "cc" 'helm-make-projectile
    "hk" 'helm-make)
  )

(use-package helm-mode-manager
             :defer t
             :init
             (evil-leader/set-key
               "hM" 'helm-switch-major-mode
               "h C-m" 'helm-enable-minor-mode))

(use-package helm-swoop
  :defer t
  :init
  (setq helm-swoop-split-with-multiple-windows t
        helm-swoop-split-direction 'split-window-vertically
        helm-swoop-speed-or-color t
        helm-swoop-split-window-function 'helm-default-display-buffer
        helm-swoop-pre-input-function (lambda () ""))
  (evil-leader/set-key
    "ss" 'helm-swoop
    "sS" 'helm-multi-swoop-all))

(use-package highlight-symbol
             :defer t
             :init
             (add-hook 'prog-mode-hook 'highlight-symbol-mode))

;; prefer helm-gtags over ggtags, ggtags is only used for references
(use-package ggtags
  :defer t
  :init
  (add-hook 'c-mode-common-hook #'ggtags-mode)
  :config
  (define-key ggtags-mode-map (kbd "M-.") nil)
  (define-key ggtags-mode-map (kbd "M-<") nil)
  (define-key ggtags-mode-map (kbd "M->") nil)
  (define-key ggtags-mode-map (kbd "M-n") nil)
  (define-key ggtags-mode-map (kbd "M-p") nil)
  (define-key ggtags-mode-map (kbd "M-,") nil)
  (define-key ggtags-mode-map (kbd "M-]") nil)
  (define-key ggtags-mode-map (kbd "M-r") 'ggtags-find-reference)
  )
(use-package helm-gtags
  :defer t
  :init
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update nil
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                (helm-gtags-mode 1))))
  :config
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "C-x 4 .") 'helm-gtags-find-tag-other-window)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack) ; override tags-loop-continue
  (define-key helm-gtags-mode-map (kbd "M-*") 'helm-gtags-resume) ; override pop-tag-mark
  )

(use-package indent-guide
             :defer t
             :init
             (setq indent-guide-delay 0.3)
             (evil-leader/set-key "ti" 'indent-guide-mode))

(use-package magit
             :defer t
             :init
             (evil-leader/set-key
               "gb" 'magit-blame
               "gl" 'magit-log-all
               "gL" 'magit-log-buffer-file
               "gs" 'magit-status
               "gd" 'spacemacs/magit-diff-head
               "gC" 'magit-commit))

(use-package neotree
  :defer t
  :init
  (setq neo-window-width 32
        neo-create-file-auto-open t
        neo-banner-message nil
        neo-show-updir-line nil
        neo-mode-line-type 'neotree
        neo-smart-open t
        neo-dont-be-alone t
        neo-persist-show nil
        neo-show-hidden-files nil
        neo-auto-indent-point t
        neo-modern-sidebar t
        neo-vc-integration '(face)
        neo-hidden-regexp-list '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$"))
  (evil-leader/set-key
    "ft" 'neotree-toggle
    "pt" 'neotree-find-project-root)
  :config
  (define-key neotree-mode-map (kbd "j") 'neotree-next-line)
  (define-key neotree-mode-map (kbd "k") 'neotree-previous-line)
  )

(use-package open-junk-file
  :init
  (setq open-junk-file-directory "/tmp/.junk/")
  (evil-leader/set-key "fJ" 'open-junk-file)
  )

(use-package pangu-spacing :defer t)

(use-package paradox :defer t)

(use-package projectile
  :init
  (setq projectile-enable-caching t
        projectile-switch-project-action 'projectile-dired)
  (evil-leader/set-key
    "pv" 'projectile-vc
    "p!" 'projectile-run-shell-command-in-root
    "p&" 'projectile-run-async-shell-command-in-root
    "pa" 'projectile-toggle-between-implementation-and-test
    "pc" 'projectile-compile-project
    "pD" 'projectile-dired
    "pG" 'projectile-regenerate-tags
    "pI" 'projectile-invalidate-cache
    "pk" 'projectile-kill-buffers
    "po" 'projectile-multi-occur
    "pR" 'projectile-replace
    "pT" 'projectile-find-test-file
    "py" 'projectile-find-tag
    )
  (projectile-global-mode t)
  )

(use-package helm-projectile
             :init
             (helm-projectile-on)
             (evil-leader/set-key
               "pb" 'helm-projectile-switch-to-buffer
               "pd" 'helm-projectile-find-dir
               "pf" 'helm-projectile-find-file
               "ph" 'helm-projectile
               "pp" 'helm-projectile-switch-project
               "pr" 'helm-projectile-recentf
               )
             )

(use-package popwin
             :commands popwin-mode
             :init (popwin-mode 1)
             :config
             (progn
               (defvar popwin:special-display-config-backup popwin:special-display-config)
               (setq display-buffer-function 'popwin:display-buffer)

               ;; basic
               (push '("*Help*" :stick t :noselect t) popwin:special-display-config)
               (push '("*helm world time*" :stick t :noselect t) popwin:special-display-config)
               (push '("*Pp Eval Output*" :stick t) popwin:special-display-config)

               ;; magit
               (push '("*magit-process*" :stick t) popwin:special-display-config)

               ;; quickrun
               (push '("*quickrun*" :stick t) popwin:special-display-config)

               ;; dictionaly
               (push '("*dict*" :stick t) popwin:special-display-config)
               (push '("*sdic*" :stick t) popwin:special-display-config)

               ;; popwin for slime
               (push '(slime-repl-mode :stick t) popwin:special-display-config)

               ;; man
               (push '(Man-mode :stick t :height 20) popwin:special-display-config)

               ;; Elisp
               (push '("*ielm*" :stick t) popwin:special-display-config)
               (push '("*eshell pop*" :stick t) popwin:special-display-config)

               ;; pry
               (push '(inf-ruby-mode :stick t :height 20) popwin:special-display-config)

               ;; python
               (push '("*Python*"   :stick t) popwin:special-display-config)
               (push '("*Python Help*" :stick t :height 20) popwin:special-display-config)
               (push '("*jedi:doc*" :stick t :noselect t) popwin:special-display-config)

               ;; Haskell
               (push '("*haskell*" :stick t) popwin:special-display-config)
               (push '("*GHC Info*") popwin:special-display-config)

               ;; sgit
               (push '("*sgit*" :position right :width 0.5 :stick t)
                     popwin:special-display-config)

               ;; git-gutter
               (push '("*git-gutter:diff*" :width 0.5 :stick t)
                     popwin:special-display-config)

               ;; es-results-mode
               (push '(es-result-mode :stick t :width 0.5)
                     popwin:special-display-config)
               
               ;; direx
               (push '(direx:direx-mode :position left :width 40 :dedicated t)
                     popwin:special-display-config)

               (push '("*Occur*" :stick t) popwin:special-display-config)

               ;; prodigy
               (push '("*prodigy*" :stick t) popwin:special-display-config)

               ;; malabar-mode
               (push '("*Malabar Compilation*" :stick t :height 30)
                     popwin:special-display-config)

               ;; org-mode
               (push '("*Org tags*" :stick t :height 30)
                     popwin:special-display-config)

               ;; Completions
               (push '("*Completions*" :stick t :noselect t) popwin:special-display-config)

               ;; ggtags
               (push '("*ggtags-global*" :stick t :noselect t :height 30) popwin:special-display-config)

               ;; async shell commands
               (push '("*Async Shell Command*" :stick t) popwin:special-display-config)

               (defun my/popup-downloads ()
                 "Pop up the downloads buffer (3rd eshell buffer for me"
                 (interactive)
                 (popwin:popup-buffer "*eshell*<3>"))

               ;; eshell 3 is always my "download stuff" buffer
               (global-set-key (kbd "C-x M-d") #'my/popup-downloads)
               (global-set-key (kbd "C-h e") 'popwin:messages)))

(use-package rainbow-delimiters
  :defer t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  )

(use-package rich-minority
  :init
  (setq rm-blacklist (quote (
                             " WS"
                             " FIC"
                             " pair"
                             " yas"
                             " Projectile"
                             " MRev"
                             " company"
                             " Undo-Tree"
                             " Anzu"
                             " Helm"
                             " hl-s"
                             " VHl"
                             " HI"
                             " HI2"
                             " Abbrev"
                             " Interactive"
                             " WK"
                             " SP"
                             " =>"
                             " Paredit"
                             ))))

(use-package saveplace :init (setq-default save-place t))

(use-package multi-term :defer t)
(use-package shell-pop
             :defer t
             :init
             (evil-leader/set-key
               "ase" 'shell-pop-eshell
               "asi" 'shell-pop-shell
               "asm" 'shell-pop-multiterm
               "ast" 'shell-pop-ansi-term))

(use-package smartparens
  :defer t
  :config
  (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
  (define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
  (define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
  (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
  (define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)
  (define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
  (define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)
  (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)
  (define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
  (define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)
  (define-key smartparens-mode-map (kbd "C-S-f") 'sp-forward-symbol)
  (define-key smartparens-mode-map (kbd "C-S-b") 'sp-backward-symbol)
  (define-key smartparens-mode-map (kbd "M-k") 'sp-backward-kill-sexp)
  (define-key smartparens-mode-map (kbd "M-]") 'sp-unwrap-sexp)
  (define-key smartparens-mode-map (kbd "M-[") 'sp-backward-unwrap-sexp)
  (define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
  (define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)
  (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

  (define-key smartparens-mode-map (kbd "C-c \"") (lambda () (interactive) (sp-wrap-with-pair "\"")))
  (define-key smartparens-mode-map (kbd "C-c '") (lambda () (interactive) (sp-wrap-with-pair "'")))
  (define-key smartparens-mode-map (kbd "C-c (") (lambda () (interactive) (sp-wrap-with-pair "(")))
  (define-key smartparens-mode-map (kbd "C-c [") (lambda () (interactive) (sp-wrap-with-pair "[")))
  (define-key smartparens-mode-map (kbd "C-c {") (lambda () (interactive) (sp-wrap-with-pair "{")))

  (sp-local-pair 'tuareg-mode "'" nil :actions nil)
  (sp-local-pair 'tuareg-mode "`" nil :actions nil)
  (sp-local-pair 'clojure-mode "'" nil :actions nil)
  (sp-local-pair 'clojure-mode "`" nil :actions nil)
  (sp-local-pair 'cider-repl-mode "'" nil :actions nil)
  (sp-local-pair 'cider-repl-mode "`" nil :actions nil)
  )

(use-package undo-tree
  :defer t
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  (global-undo-tree-mode 1)
  )

(use-package which-key :config (which-key-mode 1))

(use-package window-numbering
  :config
  (window-numbering-mode 1)
  "0" 'select-window-0
  "1" 'select-window-1
  "2" 'select-window-2
  "3" 'select-window-3
  "4" 'select-window-4
  "5" 'select-window-5
  "6" 'select-window-6
  "7" 'select-window-7
  "8" 'select-window-8
  "9" 'select-window-9
  )

(use-package yasnippet
             :config
             (setq yas-verbosity 0                  ; tone down yasnippet logging
                   yas-snippet-dirs `(,(concat user-emacs-directory "snippets")))
             (yas-global-mode 1)
             (define-key yas-minor-mode-map (kbd "<tab>")  nil) ;; let hippie-extrand trigger yasnippet
             )

(use-package ycmd
             :init
             (set-variable 'ycmd-server-command '("python2" "/home/ray/Util/ycmd/ycmd"))
             (set-variable 'ycmd-global-config "/home/ray/.config/ycmd/ycm_extra_conf.py")
             (set-variable 'ycmd-extra-conf-whitelist "/home/ray/Work/*")
             (global-ycmd-mode 1)
             )

(use-package company-ycmd
             :init
             (setq company-backends (remove 'company-clang company-backends))
             (company-ycmd-setup))

(use-package flycheck-ycmd
             :defer t
             :init
             (add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup))

(use-package zeal-at-point
  :defer t
  :init
  (advice-add 'zeal-at-point-run-search :after  (lambda (search)
                                                  (call-process-shell-command "sleep 0.2; xdotool key ctrl+apostrophe z")
                                                  ))
  (evil-leader/set-key
    "az" 'zeal-at-point))

;; Functions -------------------------------------------------------------------

(defun my/next-error (&optional n reset)
  (interactive "P")
  (if (symbol-value flycheck-mode)
      (call-interactively 'flycheck-next-error)
    (call-interactively 'next-error)))

(defun my/previous-error (&optional n reset)
  (interactive "P")
  (if (symbol-value flycheck-mode)
      (call-interactively 'flycheck-next-error)
    (call-interactively 'next-error)))

;; from magnars
(defun spacemacs/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(defmacro spacemacs|defvar-company-backends (mode)
  "Define a MODE specific company backend variable with default backends.
The variable name format is company-backends-MODE."
  `(defvar ,(intern (format "company-backends-%S" mode))
     '((company-dabbrev-code company-gtags company-etags company-keywords)
       company-files company-dabbrev)
     ,(format "Company backend list for %S" mode)))

(defmacro my|add-company-hook (mode)
  "Enable company for the given MODE.
MODE must match the symbol passed in `spacemacs|defvar-company-backends'.
The initialization function is hooked to `MODE-hook'."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "my//init-company-%S" mode)))
        (backend-list (intern (format "company-backends-%S" mode))))
    `(progn
       (defun ,func ()
         ,(format "Initialize company for %S" mode)
         (set (make-variable-buffer-local 'company-backends)
              ,backend-list))
       (add-hook ',mode-hook ',func t)
       (add-hook ',mode-hook 'company-mode t))))

;; Key bindings ----------------------------------------------------------------

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-s")		'isearch-forward-regexp)
(global-set-key (kbd "C-r")		'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
;; (global-set-key (kbd "C-x p") popwin:keymap)
(global-set-key (kbd "C-x M-e") 'eval-print-last-sexp)
(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "C-c C-r") 'realgud:gdb))
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "M-/")   'hippie-expand)
(global-set-key (kbd "<tab>") (lambda ()
                                (interactive)
                                (if (looking-at "\\>") (hippie-expand nil)
                                  (indent-for-tab-command))))

(defun my/expand-lines ()
  (interactive)
  (let ((hippie-expand-try-functions-list
         '(try-expand-line)))
    (call-interactively 'hippie-expand)))
(define-key evil-insert-state-map (kbd "C-x C-l") 'my/expand-lines)

(defun kill-region-or-backward-word ()
  "If the region is active and non-empty, call `kill-region'. Otherwise, call `backward-kill-word'."
  (interactive)
  (call-interactively
   (if (use-region-p) 'kill-region 'backward-kill-word)))
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)

;; helm bindings
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-;") 'helm-mini)
(global-set-key (kbd "M-s o") 'helm-swoop)
(define-key helm-command-map (kbd "I") 'helm-imenu)
(define-key helm-command-map (kbd "j") 'helm-register)
(define-key helm-command-map (kbd "o") 'helm-multi-swoop-current-mode)
(define-key helm-command-map (kbd "g") 'helm-do-grep-ag)

(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

(global-unset-key (kbd "C-j"))
(define-key lisp-interaction-mode-map (kbd "C-j") nil)
(global-set-key (kbd "C-j h") 'windmove-left)
(global-set-key (kbd "C-j j") 'windmove-down)
(global-set-key (kbd "C-j k") 'windmove-up)
(global-set-key (kbd "C-j l") 'windmove-right)

(loop for (prefix . name) in
      '(
        ("SPC b" . "buffers")
        ("SPC c" . "compile")
        ("SPC e" . "errors")
        ("SPC f" . "files")
        ("SPC g" . "magit")
        ("SPC h" . "helm/help")
        ("SPC m" . "major-mode-cmd")
        ("SPC p" . "projects")
        ("SPC s" . "search/symbols")
        ("SPC t" . "toggles")
        ("SPC T" . "toggles/themes")
        ("SPC w" . "windows")
        ) do (which-key-declare-prefixes prefix name))

(evil-leader/set-key
  ;; applications
  "ad" 'dired
  "ap" 'proced
  "aP" 'paradox-list-packages
  "au" 'undo-tree-visualize

  ;; +buffers
  "bd" 'kill-this-buffer
  "bk" 'kill-buffer

  ;; +compile
  "cC" 'compile
  "cr" 'recompile
  "c;" 'comment-dwim-2

  ;; +errors
  "en" 'my/next-error
  "ep" 'my/previous-error

                                        ; +files
  "fF" 'helm-find-files
  "fL" 'helm-locate
  "fR" 'spacemacs/rename-current-buffer-file
  "fS" 'evil-write-all
  "fj" 'dired-jump
  "fl" 'find-file-literally
  "fs" 'write-buffer
  "fr" 'helm-recentf

  "hb" 'helm-filtered-bookmarks
  "hi" 'helm-imenu
  "hI" 'helm-info-at-point
  "hl" 'helm-resume

  "iu" 'helm-ucs

  "rm" 'helm-all-mark-rings
  "rr" 'helm-register

  "swg" 'helm-google-suggest
  "sww" 'helm-wikipedia-suggest

  "tF" 'auto-fill-mode
  "tI" 'aggressive-indent-mode
  "tl" 'toggle-truncate-lines
  "tL" 'visual-line-mode
  "tn" 'global-linum-mode
  "tp" 'smartparens-mode
  "ts" 'flycheck-mode
  "tS" 'flyspell-mode

  "Tf" 'fringe-mode
  "Tm" 'menu-bar-mode
  "Tt" 'tool-bar-mode

  "wc"  'delete-window
  "wH"  'evil-window-move-far-left
  "w <S-left>"  'evil-window-move-far-left
  "wh"  'evil-window-left
  "w <left>"  'evil-window-left
  "wJ"  'evil-window-move-very-bottom
  "w <S-down>"  'evil-window-move-very-bottom
  "wj"  'evil-window-down
  "w <down>"  'evil-window-down
  "wK"  'evil-window-move-very-top
  "w <S-up>"  'evil-window-move-very-top
  "wk"  'evil-window-up
  "w <up>"  'evil-window-up
  "wL"  'evil-window-move-far-right
  "w <S-right>"  'evil-window-move-far-right
  "wl"  'evil-window-right
  "w <right>"  'evil-window-right
  "wo"  'other-frame
  "ws"  'split-window-below
  "wS"  'split-window-below-and-focus
  "w-"  'split-window-below
  "wU"  'winner-redo
  "wu"  'winner-undo
  "wv"  'split-window-right
  "wV"  'split-window-right-and-focus
  "ww"  'other-window
  "w/"  'split-window-right
  "w="  'balance-windows

  "u" 'universal-argument

  "xaa" 'align
  "xwc" 'count-words-region

  "?" 'helm-descbinds)

(define-key evil-normal-state-map (kbd "] l") 'my/next-error)
(define-key evil-normal-state-map (kbd "[ l") 'my/previous-error)
(define-key evil-normal-state-map (kbd "[ h") 'diff-hl-previous-hunk)
(define-key evil-normal-state-map (kbd "] h") 'diff-hl-next-hunk)
;; select pasted text
(define-key evil-normal-state-map (kbd "g p") (kbd "` [ v ` ]"))

;; Programming -----------------------------------------------------------------

(add-hook 'prog-mode-hook 'linum-mode)  ; show line numbers
(add-hook 'prog-mode-hook 'eldoc-mode)            ; always use eldoc

;; Major modes -----------------------------------------------------------------

;;;; C/C++

(defun my/c++-mode-hook ()
  (setq flycheck-gcc-language-standard "c++11")
  (setq flycheck-clang-language-standard "c++11")
  )
(add-hook 'c++-mode-hook 'my/c++-mode-hook)

(with-eval-after-load "cc-mode"
  (modify-syntax-entry ?_ "w" c-mode-syntax-table)
  (modify-syntax-entry ?_ "w" c++-mode-syntax-table))

(use-package realgud :defer t)

(setq compilation-window-height 9)
(setq-default c-basic-offset 2)

;;;; Clojure

(add-hook 'clojure-mode-hook (lambda ()
                               (smartparens-strict-mode 1)
                               (aggressive-indent-mode 1)))

(use-package cider
  :defer t
  :init
  (setq cider-prompt-for-symbol nil
        cider-show-error-buffer 'only-in-repl
        nrepl-hide-special-buffers t
        )
  (add-hook 'cider-mode-hook (lambda () (smartparens-strict-mode 1)))
  (add-hook 'cider-repl-mode-hook (lambda ()
                                    (smartparens-strict-mode 1)
                                    (rainbow-delimiters-mode 1)))
  :config
  (spacemacs|defvar-company-backends cider-mode)
  (my|add-company-hook cider-mode)
  (push 'company-capf company-backends-cider-mode)
  (my|add-company-hook cider-mode)
  (spacemacs|defvar-company-backends cider-repl-mode)
  (push 'company-capf company-backends-cider-repl-mode)

  (make-local-variable 'hippie-expand-try-functions-list)
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev-visible))
  (define-key clojure-mode-map (kbd "C-k") 'sp-kill-hybrid-sexp)
  (define-key clojure-mode-map (kbd "M-q") 'sp-indent-defun)
  (define-key clojure-mode-map (kbd "C-c J") 'cider-restart)
  (define-key clojure-mode-map (kbd "C-x M-e") 'cider-eval-print-last-sexp)
  (evil-define-key 'normal clojure-mode-map (kbd "M-.") 'cider-find-var)
  (evil-define-key 'normal clojure-mode-map (kbd "M-,") 'cider-jump-back)
  )

(use-package clj-refactor
  :defer t
  :init
  (cljr-add-keybindings-with-prefix "C-c C-f")
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-f")
  )


;;;; Coq

(require 'proof-site nil t)
(use-package company-coq
             :defer t
             :init
             (add-hook 'coq-mode-hook 'company-coq-initialize))

;;;; Emacs Lisp

(evil-leader/set-key-for-mode 'emacs-lisp-mode
  "me$" 'lisp-state-eval-sexp-end-of-line
  "meb" 'eval-buffer
  "mee" 'eval-last-sexp
  "mer" 'eval-region
  "mef" 'eval-defun
  "mel" 'lisp-state-eval-sexp-end-of-line
  "m,"  'lisp-state-toggle-lisp-state
  "mtb" 'spacemacs/ert-run-tests-buffer
  "mtq" 'ert)

;;;; Haskell

(use-package haskell-mode :defer t)
;; (use-package ghc :defer t)
(use-package shm
  :defer t
  :init
  (add-hook 'haskell-mode-hook 'structured-haskell-mode)
  :config
  (define-key shm-map (kbd "M-;") nil)
  )
(use-package company-ghc
  :defer t
  :init
  (my|add-company-hook haskell-mode)
  (spacemacs|defvar-company-backends haskell-mode)
  (push 'company-ghc company-backends-haskell-mode)
  )

;;;; Java

(use-package emacs-eclim
  :defer t
  :init
  (add-hook 'java-mode-hook 'eclim-mode)
  (setq eclim-executable (or (executable-find "eclim") "/usr/lib/eclipse/eclim")
        help-at-pt-display-when-idle t
        help-at-pt-timer-delay 0.1)
  (help-at-pt-set-timer)

  (add-to-list 'minor-mode-alist
               '(eclim-mode (:eval (eclim-modeline-string))))

  (spacemacs|defvar-company-backends java-mode)
  (my|add-company-hook java-mode)
  (push 'company-emacs-eclim company-backends-java-mode)

  (evil-define-key 'insert java-mode-map
    ;; (kbd ".") 'spacemacs/java-completing-dot
    ;; (kbd ":") 'spacemacs/java-completing-double-colon
    (kbd "M-.") 'eclim-java-find-declaration
    (kbd "M-,") 'pop-tag-mark
    (kbd "M-<mouse-3>") 'eclim-java-find-declaration
    (kbd "<mouse-8>") 'pop-tag-mark)

  (evil-define-key 'normal java-mode-map
    (kbd "M-.") 'eclim-java-find-declaration
    (kbd "M-,") 'pop-tag-mark
    (kbd "M-<mouse-3>") 'eclim-java-find-declaration
    (kbd "<mouse-8>") 'pop-tag-mark)

  (evil-define-key 'normal eclim-problems-mode-map
    (kbd "a") 'eclim-problems-show-all
    (kbd "e") 'eclim-problems-show-errors
    (kbd "g") 'eclim-problems-buffer-refresh
    (kbd "q") 'eclim-quit-window
    (kbd "w") 'eclim-problems-show-warnings
    (kbd "f") 'eclim-problems-toggle-filefilter
    (kbd "c") 'eclim-problems-correct
    (kbd "RET") 'eclim-problems-open-current)

  (evil-define-key 'normal eclim-project-mode-map
    (kbd "N") 'eclim-project-create
    (kbd "m") 'eclim-project-mark-current
    (kbd "M") 'eclim-project-mark-all
    (kbd "u") 'eclim-project-unmark-current
    (kbd "U") 'eclim-project-unmark-all
    (kbd "o") 'eclim-project-open
    (kbd "c") 'eclim-project-close
    (kbd "i") 'eclim-project-info-mode
    (kbd "I") 'eclim-project-import
    (kbd "RET") 'eclim-project-goto
    (kbd "D") 'eclim-project-delete
    (kbd "p") 'eclim-project-update
    (kbd "g") 'eclim-project-mode-refresh
    (kbd "R") 'eclim-project-rename
    (kbd "q") 'eclim-quit-window)

  (evil-leader/set-key-for-mode 'java-mode
    "mea" 'eclim-problems-show-all
    "meb" 'eclim-problems
    "mec" 'eclim-problems-correct
    "mee" 'eclim-problems-show-errors
    "mef" 'eclim-problems-toggle-filefilter
    "men" 'eclim-problems-next-same-window
    "meo" 'eclim-problems-open
    "mep" 'eclim-problems-previous-same-window
    "mew" 'eclim-problems-show-warnings

    "mff" 'eclim-java-find-generic

    "mgg" 'eclim-java-find-declaration
    "mgt" 'eclim-java-find-type

    "mrc" 'eclim-java-constructor
    "mrg" 'eclim-java-generate-getter-and-setter
    "mrf" 'eclim-java-format
    "mri" 'eclim-java-import-organize
    "mrj" 'eclim-java-implement
    "mrr" 'eclim-java-refactor-rename-symbol-at-point

    "mhc" 'eclim-java-call-hierarchy
    "mhh" 'eclim-java-show-documentation-for-current-element
    "mhi" 'eclim-java-hierarchy
    "mhu" 'eclim-java-find-references

    "mmi" 'spacemacs/java-maven-clean-install
    "mmI" 'spacemacs/java-maven-install
    "mmp" 'eclim-maven-lifecycle-phases
    "mmr" 'eclim-maven-run
    "mmR" 'eclim-maven-lifecycle-phase-run
    "mmt" 'spacemacs/java-maven-test

    "maa" 'eclim-ant-run
    "mac" 'eclim-ant-clear-cache
    "mar" 'eclim-ant-run
    "mav" 'eclim-ant-validate

    "mpb" 'eclim-project-build
    "mpc" 'eclim-project-create
    "mpd" 'eclim-project-delete
    "mpg" 'eclim-project-goto
    "mpi" 'eclim-project-import
    "mpj" 'eclim-project-info-mode
    "mpk" 'eclim-project-close
    "mpo" 'eclim-project-open
    "mpp" 'eclim-project-mode
    "mpu" 'eclim-project-update

    "mtt" 'eclim-run-junit)
  )

;;;; LaTeX

(use-package auctex :defer t :mode ("\\.tex\\'" . latex-mode))

;;;; MarkDown

(use-package markdown-mode
  :defer t
  :mode ("\\.m[k]d\\'" . markdown-mode)
  :init
  ;; Header navigation in normal state movements
  (evil-define-key 'normal markdown-mode-map
    "gj" 'outline-forward-same-level
    "gk" 'outline-backward-same-level
    "gh" 'outline-up-heading
    ;; next visible heading is not exactly what we want but close enough
    "gl" 'outline-next-visible-heading)
  )

;;;; Nim

(use-package nim-mode
  :defer t
  :init
  ; (spacemacs|defvar-company-backends nim-mode)
  ; (my|add-company-hook nim-mode)
  ; (push 'company-nim company-backends-nim-mode)
  )

;;;; OCaml

(use-package tuareg
  :defer t
  :init
  (evil-leader/set-key-for-mode 'tuareg-mode
    "mga" 'tuareg-find-alternate-file)
  :config
  (when (fboundp 'sp-local-pair)
    ;; don't auto-close apostrophes (type 'a = foo) and backticks (`Foo)
    (sp-local-pair 'tuareg-mode "'" nil :actions nil)
    (sp-local-pair 'tuareg-mode "`" nil :actions nil))
  )

(use-package merlin
  :defer t
  :init
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (spacemacs|defvar-company-backends merlin-mode)
  (my|add-company-hook merlin-mode)
  (push 'merlin-company-backend company-backends-merlin-mode)
  (setq merlin-completion-with-doc t)
  (evil-leader/set-key-for-mode 'tuareg-mode
    "mcp" 'merlin-project-check
    "mcr" 'merlin-refresh
    "mcv" 'merlin-goto-project-file
    "meC" 'merlin-error-check
    "men" 'merlin-error-next
    "meN" 'merlin-error-prev
    "mgb" 'merlin-pop-stack
    "mgg" #'(lambda ()
              (interactive)
              (let ((merlin-locate-in-new-window 'never))
                (merlin-locate)))
    "mgG" #'(lambda ()
              (interactive)
              (let ((merlin-locate-in-new-window 'always))
                (merlin-locate)))
    "mgl" 'merlin-locate-ident
    "mgi" 'merlin-switch-to-ml
    "mgI" 'merlin-switch-to-mli
    "mhh" 'merlin-document
    "mht" 'merlin-type-enclosing
    "mhT" 'merlin-type-expr
    "mrd" 'merlin-destruct
    ))

(use-package ocp-indent
  :defer t
  :init
  (add-hook 'tuareg-mode-hook 'ocp-indent-caml-mode-setup))

;;;; Org

(setq org-directory "~/org"
      org-default-notes-file "~/org/refile.org"
      org-agenda-files '("~/org/refile.org" "~/org/gtd.org" "~/org/notes.org")
      org-startup-indented t                           ; org-indent-mode
      org-treat-S-cursor-todo-selection-as-state-change nil
      org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
        (sequence "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))
      org-todo-keyword-faces
      '(("TODO" :foreground "#cc9393" :weight bold)
        ("NEXT" :foreground "#8cd0d3" :weight bold)
        ("DONE" :foreground "#afd8af" :weight bold)
        ("HOLD" :foreground "#dc8cc3" :weight bold)
        ("CANCELLED" :foreground "#bfebbf" :weight bold)))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("HOLD" ("HOLD" . t))
              (done ("HOLD"))
              ("TODO" ("CANCELLED") ("HOLD"))
              ("NEXT" ("CANCELLED") ("HOLD"))
              ("DONE" ("CANCELLED") ("HOLD")))))

(setq org-capture-templates
      '(("t" "Tasks" entry (file "~/org/refile.org")
         "* TODO %?\n  SCHEDULED: %^t")
        ("d" "Diary" entry (file+datetree "~/org/diary.org")
         "* %?")
        ("n" "Notes" entry (file "~/org/notes.org")
         "* %? :NOTE:")
        ("m" "Meeting" entry (file "~/org/refile.org")
         "* MEETING with %? :MEETING:\n%U")
        ("p" "Phone call" entry (file "~/org/refile.org")
         "* PHONE %? :PHONE:\n%U")
        ))

;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9)))
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              )))

(setq org-use-sub-superscripts nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ledger . t)
   (sh . t)))

(require 'ox-md)

;;;; Python

(use-package python-mode)

(use-package cython-mode :defer t :mode ("\\.pyx\\'" . cython-mode))

(use-package anaconda-mode
  :defer t
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  :config
  (evil-leader/set-key-for-mode 'python-mode
    "mhh" 'anaconda-mode-show-doc
    "mgg" 'anaconda-mode-find-definitions
    "mga" 'anaconda-mode-find-assignments
    "mgu" 'anaconda-mode-find-references)
  )

;;;; Ruby

(use-package robe
  :defer t
  :init
  (add-hook 'ruby-mode-hook 'robe-mode)
  (spacemacs|defvar-company-backends ruby-mode)
  (my|add-company-hook ruby-mode)
  (push 'company-robe company-backends-ruby-mode)
  :config
  (evil-leader/set-key-for-mode 'ruby-mode
    "mgg" 'robe-jump
    "mhd" 'robe-doc
    "mrsr" 'robe-rails-refresh

    "msf" 'ruby-send-definition
    "msF" 'ruby-send-definition-and-go
    "msi" 'robe-start
    "msr" 'ruby-send-region
    "msR" 'ruby-send-region-and-go
    "mss" 'ruby-switch-to-inf
    ))

(use-package ruby-tools
  :defer t
  :init
  (add-hook 'enh-ruby-mode-hook 'ruby-tools-mode)
  :config
  (progn
    (spacemacs|hide-lighter ruby-tools-mode)
    (evil-leader/set-key-for-mode 'enh-ruby-mode "mx\'" 'ruby-tools-to-single-quote-string)
    (evil-leader/set-key-for-mode 'enh-ruby-mode "mx\"" 'ruby-tools-to-double-quote-string)
    (evil-leader/set-key-for-mode 'enh-ruby-mode "mx:" 'ruby-tools-to-symbol)))

;;;; Rust

(use-package rust-mode
  :defer t
  :init
  (setq rust-indent-offset 2)
  :config
  (when (fboundp 'sp-local-pair)
        ;; Don't pair lifetime specifiers
        (sp-local-pair 'rust-mode "'" nil :actions nil))
  )

(use-package racer
  :defer t
  :init
  (add-hook 'rust-mode-hook 'racer-mode))

;;;; Shell

(setq sh-basic-offset 2)

;;;; Web

(use-package web-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (setq web-mode-enable-css-colorization t
        web-mode-enable-block-face t
        web-mode-enable-part-face t
        web-mode-enable-current-element-highlight t ; highlight current HTML element
        web-mode-markup-indent-offset 2      ; HTML indent
        web-mode-css-indent-offset 2         ; CSS indent
        web-mode-code-indent-offset 2        ; JavaScript/PHP/... indent
        )
  :config
  (define-key web-mode-map (kbd "M-;") nil)
  (mapcar (lambda (x)
            (evil-define-key 'emacs web-mode-map (kbd (car x)) (cadr x)))
          '(
            ("c" web-mode-element-clone)
            ("d" web-mode-element-vanish)
            ("D" web-mode-element-kill)
            ("j" web-mode-element-next)
            ("J" web-mode-element-sibling-next)
            ("gj" web-mode-element-sibling-next)
            ("k" web-mode-element-previous)
            ("K" web-mode-element-sibling-previous)
            ("gk" web-mode-element-sibling-previous)
            ("h" web-mode-element-parent)
            ("l" web-mode-element-child)
            ("p" web-mode-dom-xpath)
            ("r" web-mode-element-rename)
            ("q" evil-normal-state)
            ("w" web-mode-element-wrap)
            ))
  )

(use-package emmet-mode
  :defer t
  :init
  (add-hook 'css-mode 'emmet-mode)
  (add-hook 'html-mode 'emmet-mode)
  (add-hook 'web-mode 'emmet-mode)
  :config
  (evil-define-key 'insert emmet-mode-keymap (kbd "TAB") 'emmet-expand-yas)
  (evil-define-key 'insert emmet-mode-keymap (kbd "<tab>") 'emmet-expand-yas)
  (evil-define-key 'emacs emmet-mode-keymap (kbd "TAB") 'emmet-expand-yas)
  (evil-define-key 'emacs emmet-mode-keymap (kbd "<tab>") 'emmet-expand-yas)
  )

(use-package js2-mode
  :defer t
  :mode ("\\.js\\'" . js2-mode)
  :init
  (setq js2-strict-missing-semi-warning nil))

(use-package jade-mode :defer t :mode ("\\.jade\\'" . jade-mode))
(use-package less-css-mode :defer t :mode ("\\.less\\'" . less-css-mode))
(use-package livescript-mode :defer t :mode ("\\.ls\\'" . livescript-mode))
(use-package sass-mode :defer t :mode ("\\.sass\\'" . sass-mode))
(use-package scss-mode :defer t :mode ("\\.scss\\'" . scss-mode))
(use-package slim-mode :defer t :mode ("\\.[sp]lim\\'" . slim-mode))
(use-package stylus-mode :defer t :mode ("\\.styl\\'" . stylus-mode))

(use-package restclient :defer t :mode ("\\.http\\'" . restclient-mode))

(use-package tagedit ; TODO
  :defer t
  :config
  (tagedit-add-experimental-features))

;;;; YAML

(use-package yaml-mode :defer t :mode (("\\.\\(yml\\|yaml\\)\\'" . yaml-mode)))

(setq debug-on-error nil
      debug-on-quit nil)
