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
  (add-hook 'c-mode-common-hook
            (lambda ()
              (c-set-style "my-cc")
              (modify-syntax-entry ?_ "w")
              ))

  (map!
   :map (c-mode-map c++-mode-map)
   (:leader
     :n "=" #'clang-format-region
     )
   (:localleader
     :desc "breakpoint"
     :n "db" (lambda ()
               (interactive)
               (evil-open-above 1)
               (insert "volatile static int z=0;while(!z)asm(\"pause\");")
               (evil-normal-state))))
  )

(def-package! clang-format
  :commands (clang-format-region)
  )

(def-package! ccls
  :load-path "~/Dev/Emacs/emacs-ccls"
  :init (add-hook 'c-mode-common-hook #'+ccls//enable)
  :config
  ;; overlay is slow
  ;; Use https://github.com/emacs-mirror/emacs/commits/feature/noverlay
  (setq ccls-sem-highlight-method 'font-lock)
  (ccls-use-default-rainbow-sem-highlight)
  (setq ccls-extra-init-params
        '(:completion (:detailedLabel t) :xref (:container t)
                      :diagnostics (:frequencyMs 5000)))

  (require 'projectile)
  (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")

  (evil-set-initial-state 'ccls-tree-mode 'emacs)
  (set! :company-backend '(c-mode c++-mode) '(company-lsp))
  )
