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
    (rtags-enable-standard-keybindings)
    (add-hook 'c-mode-common-hook 'rtags-start-process-unless-running))
  (use-package flycheck-rtags
    :ensure rtags))

(defun my-rtags/post-init-rtags ()
  (add-to-list 'spacemacs-jump-handlers-c++-mode 'rtags-find-symbol-at-point)
  (add-to-list 'spacemacs-jump-handlers-c-mode 'rtags-find-symbol-at-point)

  (spacemacs/set-leader-keys
    "jl" (lambda ()
           (interactive)
           (if (rtags-is-indexed) (rtags-imenu) (call-interactively 'spacemacs/helm-jump-in-buffer)))

    )
  )
