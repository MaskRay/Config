(defvar spacemacs-default-reference-handlers '(helm-gtags-find-rtag)
  "List of reference handlers available in every mode.")

(defvar-local spacemacs-reference-handlers '()
  "List of reference handlers local to this buffer.")

(spacemacs|define-reference-handlers c++-mode)
(spacemacs|define-reference-handlers c-mode)
(spacemacs|define-reference-handlers d-mode)

(setq dumb-jump-default-project nil) ; Do not search ~ (default)
(setq projectile-switch-project-action 'projectile-dired)

(setq-default standard-indent 2 sh-indentation 2)
