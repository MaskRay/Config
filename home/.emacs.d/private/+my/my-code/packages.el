(defconst my-code-packages
  '(
    cc-mode
    company-lsp
    dumb-jump
    evil
    haskell-mode
    helm-xref
    (lsp-mode :location local)
    lsp-haskell
    lsp-rust
    (lsp-ui :location local)
    realgud
    smartparens
    ))

(defun my-code/post-init-cc-mode ()
  (dolist (mode c-c++-modes)
    (spacemacs/declare-prefix-for-mode mode "mx" "format")
    (spacemacs/set-leader-keys-for-major-mode mode
      "xf" 'clang-format-region
      "db" (lambda ()
             (interactive)
             (evil-open-above 1)
             (insert "volatile static int z=0;while(!z)asm(\"pause\");")
             (evil-normal-state)
             )))
  )

(defun my-code/init-company-lsp ()
  (use-package company-lsp
    :config
    (spacemacs|add-company-backends :backends company-lsp :modes c-mode-common)))

(defun my-code/post-init-dumb-jump ()
  ;; Don't use dumb-jump-go in large code base.
  (advice-add 'dumb-jump-go :around #'my-advice/dumb-jump-go))

(defun my-code/post-init-haskell-mode ()
  (with-eval-after-load 'haskell-mode
    (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
    ;; (add-hook 'haskell-mode-hook 'structured-haskell-mode)
    ;; (add-hook 'haskell-mode-hook 'lsp-mode)
    ;; (intero-global-mode 1)
    ;; (add-hook 'haskell-mode-hook 'intero-mode)
    ;; (add-to-list 'spacemacs-jump-handlers-haskell-mode 'intero-goto-definition)
    )
  )

(defun my-code/post-init-evil ()
  (add-to-list 'evil-emacs-state-modes 'xref--xref-buffer-mode)

  (define-key evil-normal-state-map "gf" 'my/ffap)
  (define-key evil-normal-state-map (kbd "C-p") 'lsp-ui-peek-jump-forward)
  (define-key evil-normal-state-map (kbd "C-t") 'lsp-ui-peek-jump-backward)
  (define-key evil-motion-state-map (kbd "M-?") 'xref-find-references)
  (define-key evil-motion-state-map (kbd "C-,")
    (defun my-xref/find-references ()
      (interactive)
      (if lsp-mode (lsp-ui-peek-find-references) (spacemacs/jump-to-definition))))
  ;; Also define M-, because C-, is unavailable in the terminal
  (define-key evil-motion-state-map (kbd "M-,") #'my-xref/find-reference)
  (define-key evil-motion-state-map (kbd "C-j")
    (defun my-xref/find-definitions ()
      (interactive)
      (if lsp-mode (lsp-ui-peek-find-definitions) (spacemacs/jump-to-definition))))
  (define-key evil-motion-state-map (kbd "M-n") 'next-error)
  (define-key evil-motion-state-map (kbd "M-p") 'previous-error)

  (spacemacs/set-leader-keys
    "aa" (lambda ()
           (interactive)
           (let ((f (file-name-base (buffer-file-name))))
             (set-buffer "*ansi-term-1*")
             (term-send-raw-string (format "\C-umake %s && ./%s \C-m" f f))))
    "ag" (lambda () (interactive) (shell-command-on-region (point-min) (point-max) "genhdr" t t))
    "aG" (lambda () (interactive) (shell-command-on-region (point-min) (point-max) "genhdr windows" t t))
    "TD" #'my/realtime-elisp-doc
    )

  (add-hook 'TeX-mode-hook #'spacemacs/toggle-auto-fill-mode-off)
  )

(defun my-code/init-helm-xref ()
  (use-package helm-xref
    :config
    ;; This is required to make xref-find-references work in helm-mode.
    ;; In helm-mode, it gives a prompt and asks the identifier (which has no text property) and then passes it to lsp-mode, which requires the text property at point to locate the references.
    ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=29619
    (setq xref-prompt-for-identifier '(not xref-find-definitions xref-find-definitions-other-window xref-find-definitions-other-frame xref-find-references spacemacs/jump-to-definition spacemacs/jump-to-reference))

    ;; Use helm-xref to display xref.el results.
    (setq xref-show-xrefs-function 'helm-xref-show-xrefs)
    )
  )

(defun my-code/init-lsp-mode ()
  (use-package lsp-mode
    :config
    (add-to-list 'spacemacs-jump-handlers-d-mode 'company-dcd-goto-definition)
    (setq lsp-enable-flycheck nil) ;; disable lsp-flycheck.el in favor of lsp-ui-flycheck.el
    ;; (setq-default flycheck-disabled-checkers '(c/c++-clang)) ;; in flycheck.el

    ;;; Override
    (dolist (mode '("c" "c++" "go" "haskell" "javascript" "python" "rust"))
      (let ((handler (intern (format "spacemacs-jump-handlers-%s-mode" mode))))
        (add-to-list handler 'lsp-ui-peek-find-definitions))
      (let ((handler (intern (format "spacemacs-reference-handlers-%s-mode" mode))))
        (add-to-list handler 'lsp-ui-peek-find-references))))
  )

(defun my-code/init-lsp-ui ()
  (use-package lsp-ui
    :after lsp-mode
    :config
    (setq lsp-ui-doc-include-signature nil)  ; don't include type signature in the child frame

    (with-eval-after-load 'lsp-mode
      (add-hook 'lsp-after-open-hook
                (lambda ()
                  (when (>= emacs-major-version 26)
                    (lsp-ui-doc-mode 1))
                  (lsp-ui-flycheck-enable 1))))

    (setq lsp-ui-peek-expand-function (lambda (xs) (mapcar #'car xs)))
    (evil-make-overriding-map lsp-ui-peek-mode-map 'normal)
    (define-key lsp-ui-peek-mode-map (kbd "h") 'lsp-ui-peek--select-prev-file)
    (define-key lsp-ui-peek-mode-map (kbd "l") 'lsp-ui-peek--select-next-file)
    (define-key lsp-ui-peek-mode-map (kbd "j") 'lsp-ui-peek--select-next)
    (define-key lsp-ui-peek-mode-map (kbd "k") 'lsp-ui-peek--select-prev)

    (setq lsp-ui-sideline-show-symbol nil)  ; don't show symbol on the right of info
    (setq lsp-ui-sideline-ignore-duplicate t)
    (set-face-attribute 'lsp-ui-sideline-symbol nil :foreground "grey30" :box nil)
    (set-face-attribute 'lsp-ui-sideline-current-symbol nil :foreground "grey38" :box nil)
    ;; (when (internal-lisp-face-p 'lsp-ui-sideline-contents)
    ;;   (set-face-attribute 'lsp-ui-sideline-contents nil :foreground "grey35")
    ;;   (set-face-attribute 'lsp-ui-sideline-current-contents nil :foreground "grey43"))

    (defun my-cquery/base () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/base"))
    (defun my-cquery/callers () (interactive) (lsp-ui-peek-find-custom 'callers "$cquery/callers"))
    (defun my-cquery/derived () (interactive) (lsp-ui-peek-find-custom 'derived "$cquery/derived"))
    (defun my-cquery/vars () (interactive) (lsp-ui-peek-find-custom 'vars "$cquery/vars"))

    (dolist (mode c-c++-modes)
      (spacemacs/set-leader-keys-for-major-mode mode
        "la" #'lsp-ui-find-workspace-symbol
        "lA" #'lsp-ui-peek-find-workspace-symbol
        "lb" #'my-cquery/base
        "lc" #'my-cquery/callers
        "ld" #'my-cquery/derived
        ;; Formatting requires waf configure --use-clang-cxx
        ;; https://github.com/jacobdufault/cquery/wiki/Formatting
        "lf" #'lsp-format-buffer
        "ll" #'lsp-ui-sideline-mode
        "lD" #'lsp-ui-doc-mode
        "ln" #'my-xref/next-reference
        "lp" #'my-xref/previous-reference
        "lr" #'lsp-rename
        "lv" #'my-cquery/vars
       ))

    (define-key evil-motion-state-map (kbd "M-<down>") 'my-xref/next-reference)
    (define-key evil-motion-state-map (kbd "M-<up>") 'my-xref/previous-reference)
    ))

(defun my-code/init-lsp-haskell ()
  (use-package lsp-haskell
    :mode ("\\.hs\\'" . haskell-mode)
    :after lsp-mode
    :config
    )
  )

(defun my-code/init-lsp-rust ()
  (use-package lsp-rust
    :after lsp-mode
    :config
    (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
    (add-hook 'rust-mode-hook
              (defun my/rust-mode-hook ()
                (lsp-rust-enable)
                (when (>= emacs-major-version 26)
                  (setq lsp-enable-eldoc nil)
                  (lsp-ui-doc-mode 1))))
    ))

(defun my-code/post-init-realgud ()
  (with-eval-after-load 'realgud
    ;; It was bound to [mouse-2], but [mouse-1] is more convenient.
    (define-key realgud:shortkey-mode-map [mouse-1] #'realgud:tooltip-eval)
    (define-key realgud:shortkey-mode-map (kbd "p") #'my/realgud-eval-region-or-word-at-point)
    (define-key realgud:shortkey-mode-map (kbd "C-o") #'evil-execute-in-normal-state)
    (define-key realgud:shortkey-mode-map (kbd "w") #'evil-forward-word-begin)
    (define-key realgud:shortkey-mode-map (kbd "W") #'evil-forward-WORD-begin)

    ;; Don't use default bindings in c-c++/packages.el
    (evilified-state-evilify-map realgud:shortkey-mode-map
      :eval-after-load realgud
      :mode realgud-short-key-mode
      :bindings
      "C-j" 'lsp-ui-peek-find-definitions
      "C-," 'lsp-ui-peek-find-references
      "C-t" 'lsp-ui-peek-jump-backward
      "C-p" 'lsp-ui-peek-jump-forward
      "n" 'realgud:cmd-next
      "s" 'realgud:cmd-step
      "b" 'realgud:cmd-break
      "B" 'realgud:cmd-clear
      "f" 'realgud:cmd-finish
      "c" 'realgud:cmd-continue
      "e" 'realgud:cmd-eval
      "r" 'realgud:cmd-restart
      "q" 'realgud:cmd-quit
      "S" 'realgud-window-cmd-undisturb-src)

    ;; Rebind 1 .. 9 to M-1 .. M-9
    ;; (define-key realgud:shortkey-mode-map (kbd "M-1") #'realgud-goto-arrow1)
    ;; (define-key realgud:shortkey-mode-map (kbd "M-2") #'realgud-goto-arrow2)
    ;; (define-key realgud:shortkey-mode-map (kbd "M-3") #'realgud-goto-arrow3)
    ;; (define-key realgud:shortkey-mode-map (kbd "M-4") #'realgud:goto-loc-hist-4)
    ;; (define-key realgud:shortkey-mode-map (kbd "M-5") #'realgud:goto-loc-hist-5)
    ;; (define-key realgud:shortkey-mode-map (kbd "M-6") #'realgud:goto-loc-hist-6)
    ;; (define-key realgud:shortkey-mode-map (kbd "M-7") #'realgud:goto-loc-hist-7)
    ;; (define-key realgud:shortkey-mode-map (kbd "M-8") #'realgud:goto-loc-hist-8)
    ;; (define-key realgud:shortkey-mode-map (kbd "M-9") #'realgud:goto-loc-hist-9)

    (define-key realgud:shortkey-mode-map (kbd "1") (lambda () (interactive) (my/realgud-eval-nth-name-forward 1)))
    (define-key realgud:shortkey-mode-map (kbd "2") (lambda () (interactive) (my/realgud-eval-nth-name-forward 2)))
    (define-key realgud:shortkey-mode-map (kbd "3") (lambda () (interactive) (my/realgud-eval-nth-name-forward 3)))
    (define-key realgud:shortkey-mode-map (kbd "4") (lambda () (interactive) (my/realgud-eval-nth-name-forward 4)))
    (define-key realgud:shortkey-mode-map (kbd "5") (lambda () (interactive) (my/realgud-eval-nth-name-forward 5)))
    (define-key realgud:shortkey-mode-map (kbd "6") (lambda () (interactive) (my/realgud-eval-nth-name-forward 6)))
    (define-key realgud:shortkey-mode-map (kbd "7") (lambda () (interactive) (my/realgud-eval-nth-name-forward 7)))
    (define-key realgud:shortkey-mode-map (kbd "8") (lambda () (interactive) (my/realgud-eval-nth-name-forward 8)))
    (define-key realgud:shortkey-mode-map (kbd "9") (lambda () (interactive) (my/realgud-eval-nth-name-forward 9)))

    (define-key realgud:shortkey-mode-map (kbd "M-1") (lambda () (interactive) (my/realgud-eval-nth-name-backward 1)))
    (define-key realgud:shortkey-mode-map (kbd "M-2") (lambda () (interactive) (my/realgud-eval-nth-name-backward 2)))
    (define-key realgud:shortkey-mode-map (kbd "M-3") (lambda () (interactive) (my/realgud-eval-nth-name-backward 3)))
    (define-key realgud:shortkey-mode-map (kbd "M-4") (lambda () (interactive) (my/realgud-eval-nth-name-backward 4)))
    (define-key realgud:shortkey-mode-map (kbd "M-5") (lambda () (interactive) (my/realgud-eval-nth-name-backward 5)))
    (define-key realgud:shortkey-mode-map (kbd "M-6") (lambda () (interactive) (my/realgud-eval-nth-name-backward 6)))
    (define-key realgud:shortkey-mode-map (kbd "M-7") (lambda () (interactive) (my/realgud-eval-nth-name-backward 7)))
    (define-key realgud:shortkey-mode-map (kbd "M-8") (lambda () (interactive) (my/realgud-eval-nth-name-backward 8)))
    (define-key realgud:shortkey-mode-map (kbd "M-9") (lambda () (interactive) (my/realgud-eval-nth-name-backward 9)))
    ))

(defun my-code/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
    (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)
    (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
    (define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
    (define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
    (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
    (define-key smartparens-mode-map (kbd "M-t") 'sp-transpose-sexp)
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
    )
  )

;;; packages.el ends here
