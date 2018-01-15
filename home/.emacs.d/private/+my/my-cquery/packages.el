(defconst my-cquery-packages '((cquery :location local)))

;; See also https://github.com/jacobdufault/cquery/wiki/Emacs
(defun my-cquery/init-cquery ()
  (use-package cquery
    :after lsp-mode
    :config
    ;; overlay is slow
    ;; Use https://github.com/emacs-mirror/emacs/commits/feature/noverlay
    (setq cquery-sem-highlight-method 'overlay)
    ;; or WAIT for https://lists.gnu.org/archive/html/emacs-devel/2017-05/msg00084.html
    (setq cquery-enable-sem-highlight t)
    (cquery-use-default-rainbow-sem-highlight)

    (setq cquery-extra-init-params '(:enableComments 2 :cacheFormat "msgpack"))
    (add-hook 'c-mode-common-hook #'my-cquery//enable)))
