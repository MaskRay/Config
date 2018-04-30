;; -*- no-byte-compile: t; -*-
;;; private/my/packages.el

(package! avy)
(package! lispyville)
(package! lsp-mode)
(package! lsp-ui)
(package! company-lsp)

(package! eshell-autojump)
(package! evil-nerd-commenter)
(package! symbol-overlay)
(package! tldr)

(package! rust-mode)
(package! lsp-rust)

(package! treemacs)
(package! treemacs-evil)
(package! treemacs-projectile)

(package! function-args)
(package! lpy :recipe (:fetcher github :repo "abo-abo/lpy" :files ("*")))
