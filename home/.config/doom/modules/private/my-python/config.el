;;; private/my-python/config.el -*- lexical-binding: t; -*-

(def-package! lsp-python
  :load-path "~/Dev/Emacs/lsp-python"
  :defer t
  :init (add-hook! python-mode #'lsp-python-enable)
  :config
  (set-company-backend! 'python-mode 'company-lsp)
  )
