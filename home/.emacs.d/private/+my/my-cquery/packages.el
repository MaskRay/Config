(defconst my-cquery-packages '((cquery :location local)))

;; See also https://github.com/jacobdufault/cquery/wiki/Emacs
(defun my-cquery/init-cquery ()
  (use-package cquery
    :after lsp-mode
    :init
    (add-hook 'c-mode-common-hook #'cquery//enable)
    :config
    ;; overlay is slow
    ;; Use https://github.com/emacs-mirror/emacs/commits/feature/noverlay
    ;; or WAIT for https://lists.gnu.org/archive/html/emacs-devel/2017-05/msg00084.html
    (setq cquery-sem-highlight-method 'overlay)
    (cquery-use-default-rainbow-sem-highlight)
    ))
