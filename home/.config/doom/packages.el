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
(package! link-hint)
(package! smart-forward)
(package! symbol-overlay)
(package! tldr)

(package! rust-mode)
(package! lsp-rust)

(package! function-args)
(package! lpy :recipe (:fetcher github :repo "abo-abo/lpy" :files ("*")))

(disable-packages! irony rtags)
