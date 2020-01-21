;; -*- no-byte-compile: t; -*-
;;; private/my/packages.el
(disable-packages! cmake-mode company-irony company-irony-c-headers flycheck-irony irony irony-eldoc ivy-rtags rtags)

(package! avy)
(package! atomic-chrome)
(package! eglot)
(package! lispyville)
(package! lsp-mode :ignore t)
(package! lsp-treemacs :ignore t)
(package! lsp-ui :ignore t)
(package! company-lsp :ignore t)
(package! spinner)                      ; required by lsp-mode

(package! eshell-autojump)
(package! evil-collection)
(package! evil-nerd-commenter)
(package! frog-jump-buffer)
(package! git-link)
(package! link-hint)
(package! htmlize)
(package! rg)
(package! smart-forward)
(package! symbol-overlay)
(package! tldr)
(package! try)

(package! d-mode)
(package! julia-repl)

(package! racer :disable t)
;; (package! rust-mode)
;; (package! lsp-rust :ignore t)

(package! function-args)
;; (package! lpy :recipe (:fetcher github :repo "abo-abo/lpy" :files ("*")))

(disable-packages! company-prescient)
