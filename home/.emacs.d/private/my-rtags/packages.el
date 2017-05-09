(defconst my-rtags-packages
  '(cmake-ide rtags))

(defun my-rtags/init-rtags ()
  (use-package rtags
    :config
    (setq rtags-autostart-diagnostics t
          rtags-completions-enabled t
          rtags-display-result-backend 'helm
          rtags-use-helm t)
                                        ; See https://github.com/Andersbakken/rtags/issues/832
    ;; (require 'rtags-helm)
    (push '(company-rtags)
          company-backends-c-mode-common)
    (rtags-enable-standard-keybindings) ;; C-c r key prefix
    (add-hook 'c-mode-common-hook 'rtags-start-process-unless-running))
  (use-package flycheck-rtags
    :ensure rtags))

(defun my-rtags/post-init-rtags ()
  (spacemacs/set-leader-keys
    "jl" (lambda ()
           (interactive)
           (if (rtags-is-indexed) (rtags-imenu) (call-interactively 'spacemacs/helm-jump-in-buffer)))

    )
  )
