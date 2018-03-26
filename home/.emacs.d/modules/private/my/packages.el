;; -*- no-byte-compile: t; -*-
;;; private/my/packages.el

(package! avy)
(package! company-lsp)
(package! lispyville)
(package! lsp-mode :recipe (:fetcher github :repo "emacs-lsp/lsp-mode"))
(package! lsp-ui :recipe (:fetcher github :repo "emacs-lsp/lsp-ui"))
