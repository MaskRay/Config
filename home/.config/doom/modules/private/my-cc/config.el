;;; private/my-cc/config.el -*- lexical-binding: t; -*-

(defvar +my/cquery-blacklist '("^/usr/") ".")

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
  (c-add-style
   "my-cc" '("user"
             (c-basic-offset . 2)
             (c-offsets-alist
              . ((innamespace . 0)
                 (access-label . -)
                 (case-label . 0)
                 (member-init-intro . +)
                 (topmost-intro . 0)
                 (arglist-cont-nonempty . +)))))
  (setq c-default-style "my-cc")

  (map!
   :map (c-mode-map c++-mode-map)
   :localleader
   "=" #'clang-format-region
   :desc "breakpoint"
   "db" (lambda ()
          (interactive)
          (evil-open-above 1)
          (insert "volatile static int z=0;while(!z)asm(\"pause\");")
          (evil-normal-state)))
  )

(def-package! clang-format
  :commands (clang-format-region)
  )

(def-package! cquery
  :load-path "~/Dev/Emacs/emacs-cquery"
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
  (set! :company-backend 'c-mode '(company-lsp))
  (set! :company-backend 'c++-mode '(company-lsp))
  )
