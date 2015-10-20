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
(setq load-prefer-newer t)
(setq mouse-wheel-follow-mouse 't)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chrome")
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq sentence-end-double-space nil)    ; single space ends a sentence
(setq-default tab-width 2
							indent-tabs-mode nil)

(setq set-mark-command-repeat-pop t)

(evil-mode 1)
(winner-mode 1)
(global-evil-surround-mode 1)
(global-diff-hl-mode)                   ; highlight uncommitted changes
(which-key-mode)                        ; display help for partial key bindings
(require 'popwin) (popwin-mode 1)       ; manage temporary windows
(electric-pair-mode t)                  ; automatically pair quotes and such
(electric-indent-mode t)                ; auto-indent things
(global-hl-line-mode)                   ; highlight the current line
(delete-selection-mode t)               ; delete selections when yanking etc
(global-aggressive-indent-mode t)       ; always aggressively indent
(setq ad-redefinition-action 'accept)   ; stop logging weird crap

;; saveplace
(require 'saveplace)
(setq-default save-place t)

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
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
;; (add-hook 'flycheck-mode-hook 'flycheck-irony-setup)
(define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck)

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

(require 'moe-theme)
(load-theme 'moe-dark t)
;; (load-theme 'zenburn t)


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

(setq sml/theme 'dark) ; make smart-mode-line respect the theme
(setq sml/no-confirm-load-theme t)
(sml/setup)

;;;; COMPANY

(require 'company)
(require 'cl)
                                        ;(define-key company-mode-map [remap hippie-expand] 'company-complete)
                                        ; (define-key company-active-map [remap hippie-expand] 'company-complete)
;; (setq company-backends (delete 'company-semantic company-backends))
(setq company-backends (set-difference company-backends '(company-semantic company-clang)))

(add-hook 'after-init-hook 'global-company-mode)

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

(global-evil-search-highlight-persist t)

;; use normal Emacs bindings in insert mode
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map (read-kbd-macro evil-toggle-key) 'evil-normal-state)
(evil-global-set-key 'insert [escape] 'evil-normal-state)
(evil-global-set-key 'insert "\C-w" 'evil-delete-backward-word)

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

;;;; C

(add-hook 'c++-mode-hook
          (lambda ()
            (setq flycheck-gcc-language-standard "c++11")
            (setq flycheck-clang-language-standard "c++11")
            ))
(eval-after-load 'cc-mode
  (lambda ()
    (modify-syntax-entry ?_ "w" c-mode-syntax-table)
    (modify-syntax-entry ?_ "w" c++-mode-syntax-table)))

(autoload 'realgud:gdb "realgud" "Invoke the gdb debugger and start the Emacs user interface." t)

(setq compilation-window-height 9)
(setq-default c-basic-offset 4)

;;;; CLOJURE

(setq cider-prompt-for-symbol nil)

;; (add-hook 'cider-mode-hook (lambda ()
;;                              (define-key evil-normal-state-local-map (kbd "M-.") 'cider-find-var)
;;                              (define-key evil-normal-state-local-map (kbd "M-,") 'cider-jump-back)
;;                              ))

(with-eval-after-load "clojure-mode"
  (define-key clojure-mode-map (kbd "C-k") 'sp-kill-hybrid-sexp)
  (make-local-variable 'hippie-expand-try-functions-list)
  (setq hippie-expand-try-functions-list '(try-expand-dabbrev))
  (define-key clojure-mode-map (kbd "C-k") 'sp-kill-hybrid-sexp)
  (define-key clojure-mode-map (kbd "M-k") 'sp-kill-sexp)
  (define-key clojure-mode-map (kbd "M-q") 'sp-indent-defun)
  (evil-define-key 'normal clojure-mode-map "D" 'sp-kill-hybrid-sexp)
  (evil-define-key 'normal clojure-mode-map (kbd "M-.") 'cider-jump-to-var)
  (evil-define-key 'normal clojure-mode-map (kbd "M-,") 'cider-jump-back)
  )

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
                                   ))

(add-to-list 'company-backends 'company-ghc)

;;;; JavaScript

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;;; Makefile

(defun my/makefile-hook ()
  (setq tab-width 8)
  )
(add-hook 'makefile-mode-hook 'my/makefile-hook)

;;;; Nim

(add-to-list 'company-backends 'company-nim)

;;;; OCaml (TODO)

(add-hook 'tuareg-mode-hook 'merlin-mode)

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

(setq css-indent-offset 2)

;;;; KEYBINDINGS
(global-set-key (kbd "C-s")		'isearch-forward-regexp)
(global-set-key (kbd "C-r")		'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-;")   'comment-dwim-2)
(global-set-key (kbd "C-x p") popwin:keymap)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c M-g") 'magit-dispatch-popup)
(global-set-key (kbd "C-c R") 'compile)
(global-set-key (kbd "C-c r") 'recompile)
(eval-after-load 'cc-mode
  '(define-key c-mode-base-map (kbd "C-c C-r") 'realgud:gdb))
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-=")   'er/expand-region)
(global-set-key (kbd "C-`")   ctl-x-r-map)
(global-set-key (kbd "M-/")   'hippie-expand)
(global-set-key (kbd "<tab>") (lambda ()
                                (interactive)
                                (if (looking-at "\\>") (hippie-expand nil)
                                  (indent-for-tab-command))))
(define-key yas-minor-mode-map (kbd "<tab>")  nil)

(global-unset-key (kbd "C-j"))
(define-key lisp-interaction-mode-map (kbd "C-j") nil)
(global-set-key (kbd "C-j h") 'windmove-left)
(global-set-key (kbd "C-j j") 'windmove-down)
(global-set-key (kbd "C-j k") 'windmove-up)
(global-set-key (kbd "C-j l") 'windmove-right)

(defun my/expand-lines ()
  (interactive)
  (let ((hippie-expand-try-functions-list
         '(try-expand-line)))
    (call-interactively 'hippie-expand)))
(define-key evil-insert-state-map (kbd "C-x C-l") 'my/expand-lines)

;; evil bindings
(define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-word-mode)
(define-key evil-motion-state-map (kbd "S-SPC") #'evil-ace-jump-char-mode)
(define-key evil-motion-state-map (kbd "M-SPC") #'evil-ace-jump-line-mode)
(define-key evil-operator-state-map (kbd "S-SPC") #'evil-ace-jump-char-mode)      ; similar to f
(define-key evil-operator-state-map (kbd "M-SPC") #'evil-ace-jump-char-to-mode)   ; similar to t
(define-key evil-operator-state-map (kbd "SPC") #'evil-ace-jump-word-mode)
(defadvice evil-visual-line (before spc-for-line-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-line-mode))
(defadvice evil-visual-char (before spc-for-char-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))
(defadvice evil-visual-block (before spc-for-char-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))

;; helm bindings
(global-set-key (kbd "C-x C-d")     'helm-browse-project)
(global-set-key (kbd "C-x C-b")     'helm-buffers-list)
(global-set-key (kbd "C-c f")       'helm-recentf)
(global-set-key (kbd "C-h b")       'helm-descbinds)
(global-set-key (kbd "C-x C-f")     'helm-find-files)
(global-set-key (kbd "M-x")         'helm-M-x)
(global-set-key (kbd "M-y")         'helm-show-kill-ring)
(global-set-key (kbd "C-c SPC")     'helm-all-mark-rings)
(global-set-key (kbd "C-x b")       'helm-mini)
(global-set-key (kbd "C-x C-f")     'helm-find-files)
(global-set-key (kbd "M-s o")       'helm-swoop)
(define-key helm-command-map (kbd "I") 'helm-imenu-in-all-buffers)
(define-key helm-command-map (kbd "j") 'helm-register)
(define-key helm-command-map (kbd "o") 'helm-multi-swoop-current-mode)
(define-key helm-command-map (kbd "g") 'helm-do-grep-ag)

(define-key global-map [remap jump-to-register]      'helm-register)
(define-key global-map [remap list-buffers]          'helm-buffers-list)
(define-key global-map [remap dabbrev-expand]        'helm-dabbrev)

(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

(with-eval-after-load "helm-gtags"
  (define-key evil-normal-state-map (kbd "M-.") nil) ; unset evil-repeat-pop-next
  (define-key evil-normal-state-map (kbd "C-t") nil) ; unset pop-tag-mark
  (define-prefix-command 'my-helm-gtags-map)
  (global-set-key (kbd "C-t") 'my-helm-gtags-map)
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack) ; override tags-loop-continue
  (define-key helm-gtags-mode-map (kbd "M-*") 'helm-gtags-resume) ; override pop-tag-mark
  (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
  (define-key my-helm-gtags-map (kbd "c") 'helm-gtags-create-tags)
  (define-key my-helm-gtags-map (kbd "f") 'helm-gtags-parse-file)
  (define-key my-helm-gtags-map (kbd "g") 'helm-gtags-find-pattern)
  (define-key my-helm-gtags-map (kbd "r") 'helm-gtags-find-rtag)
  (define-key my-helm-gtags-map (kbd "s") 'helm-gtags-find-symbol)
  (define-key my-helm-gtags-map (kbd "S") 'helm-gtags-show-stack)
  (define-key my-helm-gtags-map (kbd "t") 'helm-gtags-select)
  (define-key my-helm-gtags-map (kbd "C-o") 'helm-gtags-previous-history)
  (define-key my-helm-gtags-map (kbd "C-i") 'helm-gtags-next-history))

;; smartparens bindings

(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)
(define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)
(define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)
(define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)
(define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)
(define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)
(define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)
(define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)
(define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
(define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
(define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

(defun my/wrap-with-paren (&optional arg)
  (interactive "p")
  (sp-select-next-thing-exchange arg)
  (execute-kbd-macro (kbd "(")))
(define-key smartparens-mode-map (kbd "C-(") 'my/wrap-with-paren)

;;;; ORG

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

;;;; PROJECTILE

(setq projectile-enable-caching t
      projectile-switch-project-action 'projectile-dired)
(projectile-global-mode t)

;;;; SMARTPARENS

(require 'smartparens-config)
(smartparens-global-mode 1)
;; (add-hook 'lisp-mode-hook #'smartparens-mode)
;; (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
;; (add-hook 'clojure-mode-hook #'smartparens-mode)
;; (add-hook 'cider-repl-mode-hook #'smartparens-mode)

;;;; YASNIPPET

(setq yas-snippet-dirs `(,(concat user-emacs-directory "snippets")))
(yas-global-mode 1)
