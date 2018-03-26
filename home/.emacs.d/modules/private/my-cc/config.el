;;; private/my-cc/config.el -*- lexical-binding: t; -*-

(after! cc-mode
  ;; https://github.com/radare/radare2
  (c-add-style
   "radare2"
   '((c-basic-offset . 2)
     (indent-tabs-mode . t)
     (c-auto-align-backslashes . nil)
     (c-offsets-alist
      (arglist-intro . ++)
      (arglist-cont . ++)
      (arglist-cont-nonempty . ++)
      (statement-cont . ++)
      )))

  (map!
   :map (c-mode-map c++-mode-map)
   :localleader
   "=" #'clang-format-region)
  )

(def-package! clang-format
  :commands (clang-format-region)
  )

(def-package! cquery
  :init (add-hook 'c-mode-common-hook #'+cquery//enable)
  :config
  ;; overlay is slow
  ;; Use https://github.com/emacs-mirror/emacs/commits/feature/noverlay
  (setq cquery-sem-highlight-method 'font-lock)
  (cquery-use-default-rainbow-sem-highlight)
  (setq cquery-extra-init-params
        '(:cacheFormat "msgpack" :completion (:detailedLabel t) :xref (:container t)
                       :diagnostics (:frequencyMs 5000)))

  (require 'projectile)
  (add-to-list 'projectile-globally-ignored-directories ".cquery_cached_index")

  (setq cquery-project-roots '("~/Dev/llvm-project" "~/Dev/llvm"))

  (evil-set-initial-state 'cquery-tree-mode 'emacs)
  (set! :company-backend 'c-mode '(company-lsp company-yasnippet))
  (set! :company-backend 'c++-mode '(company-lsp company-yasnippet))
  )
