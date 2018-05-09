;; -*- no-byte-compile: t; -*-
;;; private/my/packages.el
(disable-packages! cmake-mode company-irony company-irony-c-headers flycheck-irony irony irony-eldoc ivy-rtags rtags)

(package! avy)
(package! lispyville)
(package! lsp-mode)
(package! lsp-ui)
(package! company-lsp)

(package! eshell-autojump)
(package! evil-nerd-commenter)
(package! smart-forward)
(package! symbol-overlay)
(package! tldr)

(package! rust-mode)
(package! lsp-rust)

(package! treemacs)
(package! treemacs-evil)
(package! treemacs-projectile)

(package! function-args)
(package! lpy :recipe (:fetcher github :repo "abo-abo/lpy" :files ("*")))

(package! irony :disable t)
(package! rtags :disable t)
