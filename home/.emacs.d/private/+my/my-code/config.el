(defvar spacemacs-default-reference-handlers '(helm-gtags-find-rtag)
  "List of reference handlers available in every mode.")

(defvar-local spacemacs-reference-handlers '()
  "List of reference handlers local to this buffer.")

(spacemacs|define-reference-handlers c++-mode)
(spacemacs|define-reference-handlers c-mode)
(spacemacs|define-reference-handlers d-mode)
(spacemacs|define-reference-handlers go-mode)
(spacemacs|define-reference-handlers javascript-mode)
(spacemacs|define-reference-handlers haskell-mode)
(spacemacs|define-reference-handlers python-mode)
(spacemacs|define-reference-handlers rust-mode)

(spacemacs|define-jump-handlers c++-mode)
(spacemacs|define-jump-handlers c-mode)
(spacemacs|define-jump-handlers d-mode)
(spacemacs|define-jump-handlers go-mode)
(spacemacs|define-jump-handlers javascript-mode)
(spacemacs|define-jump-handlers go-mode)
(spacemacs|define-jump-handlers python-mode)
(spacemacs|define-jump-handlers rust-mode)

(setq dumb-jump-default-project nil) ; Do not search ~ (default)
(setq projectile-switch-project-action 'projectile-dired)

(setq-default standard-indent 2 sh-indentation 2)

(defvar my-xref-blacklist nil
  "List of paths that should not enable xref-find-* or dumb-jump-go")
