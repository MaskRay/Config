(defconst my-rtags-packages
  '(flycheck-rtags helm-rtags rtags))

(defun my-rtags/init-rtags ()
  (use-package rtags
    :config
    (setq rtags-autostart-diagnostics t
          rtags-completions-enabled t
          rtags-display-result-backend 'helm
          rtags-use-bookmarks nil
          rtags-use-helm t)
    (push '(company-rtags)
          company-backends-c-mode-common)
    (rtags-enable-standard-keybindings) ;; C-c r key prefix
    (add-hook 'c-mode-common-hook 'rtags-start-process-unless-running))
  (use-package flycheck-rtags
    :ensure rtags))

(defun my-rtags/init-flycheck-rtags ()
  (use-package flycheck-rtags))

(defun my-rtags/init-helm-rtags ()
  (use-package helm-rtags))

(defun my-rtags/post-init-rtags ()
  (spacemacs/set-leader-keys
    "jl" (lambda ()
           (interactive)
           (if (rtags-is-indexed) (rtags-imenu) (call-interactively 'spacemacs/helm-jump-in-buffer)))

    )
  )
