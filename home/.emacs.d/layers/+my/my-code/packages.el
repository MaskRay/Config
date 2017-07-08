(defconst my-code-packages
  '(
    cc-mode
    evil
    haskell-mode
    smartparens
    ycmd
    ))

(defun my-code/init-my-code ()
  ;; (message "+++ my-code/init-my-code")
  ;; (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
  ;; (add-hook 'c-mode-local-vars-hook #'spacemacs/ggtags-mode-enable)

  (helm-autoresize-mode 1)
  )

(defun my-code/post-init-cc-mode ()
  ;; Take priority over ycmd
  (with-eval-after-load 'ycmd
    (add-to-list 'spacemacs-jump-handlers-c++-mode 'rtags-find-symbol-at-point)
    (add-to-list 'spacemacs-jump-handlers-c-mode 'rtags-find-symbol-at-point)
    (add-to-list 'spacemacs-jump-handlers-d-mode 'company-dcd-goto-definition)
    (add-to-list 'spacemacs-reference-handlers-c++-mode 'rtags-find-references-at-point)
    (add-to-list 'spacemacs-reference-handlers-c-mode 'rtags-find-references-at-point)
    )
  )

(defun my-code/post-init-haskell-mode ()
  (with-eval-after-load 'haskell-mode
    (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
    (add-hook 'haskell-mode-hook 'structured-haskell-mode)
    ;; (intero-global-mode 1)
    (add-hook 'haskell-mode-hook 'helm-kythe-mode)
    (add-hook 'haskell-mode-hook 'intero-mode)
    (add-to-list 'spacemacs-jump-handlers-haskell-mode 'intero-goto-definition)
    (add-to-list 'spacemacs-jump-handlers-haskell-mode 'helm-kythe-find-definitions)
    (add-to-list 'spacemacs-reference-handlers-haskell-mode 'helm-kythe-find-references))
  (load "~/Dev/Emacs/emacs-helm-kythe/helm-kythe.el" t)  ;; TODO
  (spacemacs/set-leader-keys-for-major-mode 'haskell-mode "k" helm-kythe-map)
  )

(defun my-code/post-init-evil ()
  (add-to-list 'evil-emacs-state-modes 'xref--xref-buffer-mode)

  (define-key evil-normal-state-map "gf" 'my-ffap)
  (define-key evil-normal-state-map (kbd "C-t") 'my-xref-jump-backward)
  (define-key evil-motion-state-map (kbd "C-,") 'spacemacs/jump-to-reference)
  (define-key evil-motion-state-map (kbd "C-]") 'my-find-tag)
  (define-key evil-motion-state-map (kbd "C-j") 'spacemacs/jump-to-definition)

  (spacemacs/set-leader-keys
    "aa" (lambda ()
           (interactive)
           (let ((f (file-name-base (buffer-file-name))))
             (set-buffer "*ansi-term-1*")
             (term-send-raw-string (format "\C-umake %s && ./%s \C-m" f f))))
    "ag" (lambda () (interactive) (shell-command-on-region (point-min) (point-max) "genhdr" t t))
    "aG" (lambda () (interactive) (shell-command-on-region (point-min) (point-max) "genhdr windows" t t))
    "TD" #'my-realtime-elisp-doc
    )
  )

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

(defun my-code/pre-init-ycmd ()
  (setq ycmd-server-command '("python" "/usr/share/vim/vimfiles/third_party/ycmd/ycmd"))
  (setq ycmd-global-config (file-truename "~/.config/ycmd/.ycm_extra_conf.py"))
  (setq ycmd-extra-conf-whitelist '("~/Dev/*" "~/CC/*"))
  )

(defun my-code/post-init-ycmd ()
  (define-key evil-normal-state-map (kbd "Y") nil)
  (spacemacs/set-leader-keys
    "Yc" 'ycmd-goto-declaration
    "Yf" 'ycmd-goto-definition
    "Yr" 'ycmd-goto-references
    "Yt" 'ycmd-get-type
    "YY" 'ycmd-goto
    )
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-checker 'ycmd)))
  )

;;; packages.el ends here
