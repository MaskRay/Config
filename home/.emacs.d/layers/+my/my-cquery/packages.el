(defconst my-cquery-packages '(lsp-mode))

(defun my-cquery/post-init-lsp-mode ()
  (add-to-load-path (expand-file-name "~/Dev/Util/cquery/emacs"))
  (use-package lsp-cquery
    :after lsp-mode
    :config
    (setq cquery/root-dir (expand-file-name "~/Dev/Util/cquery/"))
    (add-hook 'c-mode-common-hook #'my//enable-cquery-if-compile-commands-json))
  ;; (spacemacs/set-leader-keys
  ;;   "jl" (lambda ()
  ;;          (interactive)
  ;;          (if (cquery-is-indexed) (cquery-imenu) (call-interactively 'spacemacs/helm-jump-in-buffer)))
  ;;   )
  )
