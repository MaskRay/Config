;;; private/my-julia/config.el -*- lexical-binding: t; -*-

(after! julia
  (add-hook! julia-mode #'lsp)
  )

(use-package! lsp-julia
  :load-path "~/Dev/Emacs/lsp-julia"
  :hook ((julia-mode) . lsp)
  :init
  (setq lsp-julia-default-environment "~/.julia/environments/v1.3")
  ;; Use global projects.
  (setq lsp-julia-package-dir nil)
  )
