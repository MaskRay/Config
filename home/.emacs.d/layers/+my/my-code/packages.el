(defconst my-code-packages
  '(
    cc-mode
    evil
    haskell-mode
    helm-xref
    lsp-mode
    lsp-haskell
    lsp-rust
    lsp-ui
    realgud
    smartparens
    ))

(defun my-code/post-init-cc-mode ()
  )

(defun my-code/init-my-code ()
  ;; (message "+++ my-code/init-my-code")
  ;; (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
  ;; (add-hook 'c-mode-local-vars-hook #'spacemacs/ggtags-mode-enable)
  ;; does not work
  )

(defun my-code/post-init-haskell-mode ()
  (with-eval-after-load 'haskell-mode
    (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
    (add-hook 'haskell-mode-hook 'structured-haskell-mode)
    ;; (add-hook 'haskell-mode-hook 'lsp-mode)
    ;; (intero-global-mode 1)
    ;; (add-hook 'haskell-mode-hook 'helm-kythe-mode)
    ;; (add-hook 'haskell-mode-hook 'intero-mode)
    ;; (add-to-list 'spacemacs-jump-handlers-haskell-mode 'intero-goto-definition)
    ;; (add-to-list 'spacemacs-jump-handlers-haskell-mode 'helm-kythe-find-definitions)
    ;; (add-to-list 'spacemacs-reference-handlers-haskell-mode 'helm-kythe-find-references)
    )
  ;; (load "~/Dev/Emacs/emacs-helm-kythe/helm-kythe.el" t)  ;; TODO
  ;; (spacemacs/set-leader-keys-for-major-mode 'haskell-mode "k" helm-kythe-map)
  )

(defun my-code/post-init-evil ()
  (add-to-list 'evil-emacs-state-modes 'xref--xref-buffer-mode)

  (define-key evil-normal-state-map "gf" 'my/ffap)
  (define-key evil-normal-state-map (kbd "C-p") 'my-xref/jump-forward)
  (define-key evil-normal-state-map (kbd "C-t") 'my-xref/jump-backward)
  (define-key evil-motion-state-map (kbd "C-,") 'spacemacs/jump-to-reference)
  ;; C-, is unavailable in terminal
  (define-key evil-motion-state-map (kbd "M-,") 'spacemacs/jump-to-reference)
  (define-key evil-motion-state-map (kbd "C-]") 'my/find-tag)
  (define-key evil-motion-state-map (kbd "C-j") 'spacemacs/jump-to-definition)
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
    (setq xref-prompt-for-identifier '(not xref-find-definitions xref-find-definitions-other-window xref-find-definitions-other-frame xref-find-references spacemacs/jump-to-definition spacemacs/jump-to-reference))

    (setq xref-show-xrefs-function 'helm-xref-show-xrefs)
    )
  )

(defun my-code/init-lsp-mode ()
  (use-package lsp-mode
    :config
    (add-to-list 'spacemacs-jump-handlers-c++-mode 'rtags-find-symbol-at-point)
    (add-to-list 'spacemacs-jump-handlers-c-mode 'rtags-find-symbol-at-point)
    (add-to-list 'spacemacs-jump-handlers-d-mode 'company-dcd-goto-definition)
    (add-to-list 'spacemacs-reference-handlers-c++-mode 'rtags-find-references-at-point)
    (add-to-list 'spacemacs-reference-handlers-c-mode 'rtags-find-references-at-point)

    (dolist (mode '("c" "c++" "go" "haskell" "javascript" "python" "rust"))
      (let ((handler (intern (format "spacemacs-jump-handlers-%s-mode" mode))))
        (add-to-list handler 'my-xref/find-definitions))
      (let ((handler (intern (format "spacemacs-reference-handlers-%s-mode" mode))))
        (add-to-list handler 'my-xref/find-references))
      ))
  )

(defun my-code/init-lsp-ui ()
  ;; (use-package lsp-line
  ;;   :after lsp-mode
  ;;   :config
  ;;   )
  (use-package lsp-ui
    :after lsp-mode
    :config
    (set-face-attribute 'lsp-line-symbol nil :foreground "grey30" :box nil)
    (set-face-attribute 'lsp-line-current-symbol nil :foreground "grey38" :box nil)
    (when (internal-lisp-face-p 'lsp-line-contents)
      (set-face-attribute 'lsp-line-contents nil :foreground "grey35")
      (set-face-attribute 'lsp-line-current-contents nil :foreground "grey43"))
    (define-key evil-normal-state-map (kbd ",ll") #'lsp-line-mode)
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
    :mode ("\\.rs\\'" . rust-mode)
    :after lsp-mode
    :config
    (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
    )
  )

(defun my-code/post-init-realgud ()
  (with-eval-after-load 'realgud
    ;; It was bound to [mouse-2], but [mouse-1] is more convenient.
    (define-key realgud:shortkey-mode-map [mouse-1] #'realgud:tooltip-eval)
    (define-key realgud:shortkey-mode-map (kbd "p") #'my/realgud-eval-region-or-word-at-point)
    (define-key realgud:shortkey-mode-map (kbd "C-o") #'evil-execute-in-normal-state)
    (define-key realgud:shortkey-mode-map (kbd "w") #'evil-forward-word-begin)
    (define-key realgud:shortkey-mode-map (kbd "W") #'evil-forward-WORD-begin)

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
