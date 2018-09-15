;;; private/my-python/config.el -*- lexical-binding: t; -*-

(def-package! lsp-python
  :init (add-hook! python-mode #'lsp-python-enable)
  :config
  (set-company-backend! 'python-mode 'company-lsp)
  )
