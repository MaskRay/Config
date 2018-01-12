(defconst my-cquery-packages '((cquery :location local)))

;; See also https://github.com/jacobdufault/cquery/wiki/Emacs
(defun my-cquery/init-cquery ()
  (require 'lsp-imenu)
  (use-package cquery
    :after lsp-mode
    :config
    ;; overlay is slow
    ;; WAIT https://lists.gnu.org/archive/html/emacs-devel/2017-05/msg00084.html
    (setq cquery-sem-highlight-method 'font-lock)
    ;; cquery.cl cquery--publich-semantic-highlighting is very slow
    (setq cquery-enable-sem-highlight t)
    (setq cquery-extra-init-params '(:enableComments 2 :cacheFormat "msgpack"))
    (add-hook 'c-mode-common-hook #'my-cquery//enable)))
