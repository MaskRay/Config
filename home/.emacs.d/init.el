                                        ; https://github.com/codahale/emacs.d/blob/master/init.el

(require 'cask)
(cask-initialize)

(add-to-list 'load-path (concat user-emacs-directory "config"))

;;;; Global
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
(setq inhibit-splash-screen t)          ; don't show the splash screen
(setq inhibit-startup-message t)        ; don't show startup messages
(setq inhibit-startup-echo-area-message t) ; don't echo anything
(prefer-coding-system 'utf-8)
(defalias 'yes-or-no-p 'y-or-n-p)       ; accept "y" for "yes"
(setq ring-bell-function 'ignore)       ; don't blink constantly
(setq require-final-newline t)          ; always add a final newline
(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chrome")
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq sentence-end-double-space nil)    ; single space ends a sentence
(setq-default tab-width 2)

(setq set-mark-command-repeat-pop t)

(ido-mode -1)
(evil-mode 1)
(winner-mode 1)
(global-evil-surround-mode 1)
(global-diff-hl-mode)                   ; highlight uncommitted changes
(which-key-mode)                        ; display help for partial key bindings
(require 'popwin) (popwin-mode 1)       ; manage temporary windows
(global-anzu-mode t)                    ; show total # of matches in modeline
(electric-pair-mode t)                  ; automatically pair quotes and such
(electric-indent-mode t)                ; auto-indent things
(global-discover-mode t)                ; add contextual menus for things
(global-hl-line-mode)                   ; highlight the current line
(delete-selection-mode t)               ; delete selections when yanking etc
(global-aggressive-indent-mode t)       ; always aggressively indent
(setq ad-redefinition-action 'accept)   ; stop logging weird crap

;; undo-tree-mode
(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t
      undo-tree-visualizer-diff t)

;; prog-mode specifics
(add-hook 'prog-mode-hook 'linum-mode)  ; show line numbers
(add-hook 'prog-mode-hook 'column-number-mode) ; show column numbers
(add-hook 'prog-mode-hook 'fic-mode)           ; highlight TODOs
(add-hook 'prog-mode-hook 'highlight-symbol-mode) ; highlight current symbol
(add-hook 'prog-mode-hook 'eldoc-mode)            ; always use eldoc
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode) ; enable rainbow delimiters

;; irony
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async)
  (add-to-list 'company-backends 'company-irony)
  ;; eldoc
  (irony-eldoc t)
  )
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; helm-gtags
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'java-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; use whitespace mode, and mark lines longer than 80 characters
(require 'whitespace)
(global-whitespace-mode)
(setq whitespace-style '(face empty lines-tail trailing))
(setq whitespace-line-column 80)
(setq whitespace-global-modes '(not git-commit-mode))

;; also fill paragraphs to 80 characters
(setq-default fill-column 80)
(setq-default whitespace-line-column 80)

;; enable flycheck everywhere
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;;;; Look
(set-face-attribute 'default nil
                    :family "Fantasque Sans Mono"
                    :height 180)
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                    (font-spec :family "Source Han Sans TC Normal" :size 20)
                    )
  )

(defun frame-setting ()
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (when (window-system)
    (menu-bar-mode 1))
  (when (not (window-system))
    (menu-bar-mode -1)
    (xterm-mouse-mode +1))
  )
(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (frame-setting))))
  (frame-setting))

(load-theme 'zenburn t)


(global-git-gutter-mode +1)
(setq-default indicate-buffer-boundaries 'left)
(setq-default indicate-empty-lines +1)

(require 'magit)


;; rich-minority
(require 'rich-minority)                ; don't show all the damn minor modes
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
                           )))

(setq sml/theme 'respectful) ; make smart-mode-line respect the theme
(setq sml/no-confirm-load-theme t)
(sml/setup)

;;;; COMPANY

(require 'company)
(require 'cl)
                                        ;(define-key company-mode-map [remap hippie-expand] 'company-complete)
                                        ; (define-key company-active-map [remap hippie-expand] 'company-complete)
;; (setq company-backends (delete 'company-semantic company-backends))
(setq company-backends (set-difference company-backends '(company-semantic company-clang)))
;; (define-key c-mode-map  [(tab)] 'company-complete)
;; (define-key c++-mode-map  [(tab)] 'company-complete)

;; (setq tab-always-indent 'complete)
;; (define-key company-mode-map [remap indent-for-tab-command]
;; 'company-indent-for-tab-command)
;; 
;; (defvar completion-at-point-functions-saved nil)
;; 
;; (defun company-indent-for-tab-command (&optional arg)
;; (interactive "P")
;; (let ((completion-at-point-functions-saved completion-at-point-functions)
;; (completion-at-point-functions '(company-complete-common-wrapper)))
;; (indent-for-tab-command arg)))
;; 
;; (defun company-complete-common-wrapper ()
;; (let ((completion-at-point-functions completion-at-point-functions-saved))
;; (company-complete-common)))

(add-hook 'after-init-hook 'global-company-mode)

(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
        try-expand-all-abbrevs
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; company-coq
(require 'proof-site)
(add-hook 'coq-mode-hook #'company-coq-initialize)

(require 'yasnippet)
(setq yas-verbosity 0)                  ; tone down yasnippet logging

;;;; EVIL
(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

(loop for (mode . state) in '((inferior-emacs-lisp-mode . emacs)
                              (nrepl-mode . insert)
                              (pylookup-mode . emacs)
                              (comint-mode . normal)
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

(global-evil-search-highlight-persist t)

;; use normal Emacs bindings in insert mode
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map (read-kbd-macro evil-toggle-key) 'evil-normal-state)
(define-key evil-insert-state-map [escape] 'evil-normal-state)

;;;; HELM

(require 'helm)
(require 'helm-config)
(require 'helm-files)
(require 'helm-net)
(require 'projectile)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(when (executable-find "curl")
  (setq helm-net-prefer-curl t))

(setq
 helm-idle-delay                        0.0
 helm-quick-update                      t
 thelm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
 helm-move-to-line-cycle-in-source      t ; move to end or beginning of source when reaching top or bottom of source.
 helm-ff-skip-boring-files              t
 helm-ff-search-library-in-sexp         t ; search for library in `require' and `declare-function' sexp.
 helm-scroll-amount                     8 ; scroll 8 lines other window using M-<next>/M-<prior>
 helm-ff-file-name-history-use-recentf  t
 helm-ff-transformer-show-only-basename nil
 helm-adaptive-history-file             (concat user-emacs-directory "helm-history"))

(helm-descbinds-install)                ; integrate w/ describe-bindings
(helm-projectile-on)                    ; integrate w/ projectile
(helm-adaptive-mode t)                  ; use adaptive mode to rank common items
(helm-mode 1)                           ; enable helm!

;;;; Haskell (TODO)

(add-hook 'haskell-mode-hook 'turn-on-hi2)
(add-hook 'haskell-mode-hook #'hindent-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(defun coda-haskell-pointfree-region ()
  "Executes the Haskell pointfree too on the marked region."
  (interactive)
  (let ((pfcmd (format "pointfree %s"
                       (shell-quote-argument (buffer-substring-no-properties
                                              (region-beginning)
                                              (region-end))))))
    (message (format "%s" (shell-command-to-string pfcmd)))))

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(require 'haskell-mode)
(require 'haskell-cabal)

(eval-after-load 'haskell-mode '(progn
                                  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
                                  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
                                  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
                                  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
                                  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
                                  (define-key haskell-mode-map (kbd "C-c C-p") 'coda-haskell-pointfree-region)))
(eval-after-load 'haskell-cabal '(progn
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
                                   (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
                                   (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

(add-to-list 'company-backends 'company-ghc)

;;;; JavaScript

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;;; Nim

(add-to-list 'company-backends 'company-nim)

;;;; Ruby (TODO)

(add-hook 'ruby-mode-hook 'robe-mode)
(push 'company-robe company-backends)

;;;; Web (TODO)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-enable-css-colorization t
      web-mode-enable-block-face t
      web-mode-enable-part-face t
      web-mode-enable-current-element-highlight t ; highlight current HTML element
      web-mode-markup-indent-offset 2      ; HTML indent
      web-mode-css-indent-offset 2         ; CSS indent
      web-mode-code-indent-offset 2        ; JavaScript/PHP/... indent
      )

;;;; KEYBINDINGS

(global-set-key (kbd "M-;")         'comment-dwim-2)
(global-set-key (kbd "M-\\")        'ggtags-find-tag-dwim)
(global-set-key (kbd "C-c c")       'compile)
(global-set-key (kbd "C-c g")       'magit-status)
(global-set-key (kbd "C-c l p")     'list-packages)
(global-set-key (kbd "C-c r")       'recompile)
(global-set-key (kbd "C-c t")       'coda/visit-term-buffer)
(global-set-key (kbd "C-=")         'er/expand-region)
(define-key yas-minor-mode-map [(tab)]        nil)
(define-key yas-minor-mode-map (kbd "TAB")    nil)
(define-key yas-minor-mode-map [backtab]     'yas-expand)


(defun my/expand-lines ()
  (interactive)
  (let ((hippie-expand-try-functions-list
         '(try-expand-line)))
    (call-interactively 'hippie-expand)))
(define-key evil-insert-state-map (kbd "C-x C-l") 'my/expand-lines)
;; (defun indent-or-complete ()
;;   (interactive)
;;   (if (looking-at "\\_>")
;;       (company-complete-common)
;;     (indent-according-to-mode)))
; (global-set-key (kbd "TAB") 'indent-or-complete)

;; evil bindings
(define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode)
(define-key evil-motion-state-map (kbd "C-SPC") #'evil-ace-jump-word-mode)
(define-key evil-operator-state-map (kbd "SPC") #'evil-ace-jump-char-mode)      ; similar to f
(define-key evil-operator-state-map (kbd "C-SPC") #'evil-ace-jump-char-to-mode) ; similar to t
(define-key evil-operator-state-map (kbd "M-SPC") #'evil-ace-jump-word-mode)
(defadvice evil-visual-line (before spc-for-line-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-line-mode))
(defadvice evil-visual-char (before spc-for-char-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))
(defadvice evil-visual-block (before spc-for-char-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))

;; helm bindings
(global-set-key (kbd "C-c M-x")     'execute-extended-command) ; old M-x
(global-set-key (kbd "C-x C-d")     'helm-browse-project)
(global-set-key (kbd "C-x C-b")     'helm-buffers-list)
(global-set-key (kbd "C-c f")       'helm-recentf)
(global-set-key (kbd "C-h b")       'helm-descbinds)
(global-set-key (kbd "C-x C-f")     'helm-find-files)
(global-set-key (kbd "M-x")         'helm-M-x)
(global-set-key (kbd "M-y")         'helm-show-kill-ring)
(global-set-key (kbd "C-c I")       'helm-imenu)
(global-set-key (kbd "C-c i")       'helm-imenu-in-all-buffers)
(global-set-key (kbd "C-c j")       'helm-register)
(global-set-key (kbd "C-c O")       'helm-swoop)
(global-set-key (kbd "C-c o")       'helm-multi-swoop-all)
(global-set-key (kbd "C-c SPC")     'helm-all-mark-rings)
(global-set-key (kbd "C-x b")       'helm-mini)
(global-set-key (kbd "C-x C-f")     'helm-find-files)
(define-key helm-command-map (kbd "o") 'helm-occur)
(define-key helm-command-map (kbd "g") 'helm-do-grep-ag)

(define-key global-map [remap jump-to-register]      'helm-register)
(define-key global-map [remap list-buffers]          'helm-buffers-list)
(define-key global-map [remap dabbrev-expand]        'helm-dabbrev)

(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

(eval-after-load "helm-gtags"
  '(progn
     (define-key evil-normal-state-map (kbd "M-.") nil) ; unset evil-repeat-pop-next
     (define-key evil-normal-state-map (kbd "C-t") nil) ; unset pop-tag-mark
     (define-prefix-command 'my-helm-gtags-map)
     (global-set-key (kbd "C-t") 'my-helm-gtags-map)
     (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-resume) ; override tags-loop-continue
     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-*") 'helm-gtags-pop-stack) ; override pop-tag-mark
     (define-key my-helm-gtags-map (kbd "c") 'helm-gtags-create-tags)
     (define-key my-helm-gtags-map (kbd "f") 'helm-gtags-parse-file)
     (define-key my-helm-gtags-map (kbd "g") 'helm-gtags-find-pattern)
     (define-key my-helm-gtags-map (kbd "r") 'helm-gtags-find-rtag)
     (define-key my-helm-gtags-map (kbd "s") 'helm-gtags-find-symbol)
     (define-key my-helm-gtags-map (kbd "S") 'helm-gtags-show-stack)
     (define-key my-helm-gtags-map (kbd "t") 'helm-gtags-select)
     (define-key my-helm-gtags-map (kbd "C-o") 'helm-gtags-previous-history)
     (define-key my-helm-gtags-map (kbd "C-i") 'helm-gtags-next-history)))

;;;; PROJECTILE

(setq projectile-enable-caching t
      projectile-switch-project-action 'projectile-dired)
(projectile-global-mode t)

;;;; SKEWER

(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

;;;; SMARTPARENS

(require 'smartparens-config)
(smartparens-global-mode 1)

;;;; YASNIPPET

(setq yas-snippet-dirs `(,(concat user-emacs-directory "snippets"))
      yas-expand-only-for-last-commands '(self-insert-command)
      )
(yas-global-mode 1)
(put 'set-goal-column 'disabled nil)
