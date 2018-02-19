(defconst cquery-packages
  '(
    (cquery :location local)
    company
    ))

;; See also https://github.com/cquery-project/cquery/wiki/Emacs
(defun cquery/init-cquery ()
  (use-package cquery
    :init
    (add-hook 'c-mode-common-hook #'cquery//enable)
    :config
    ;; overlay is slow
    ;; Use https://github.com/emacs-mirror/emacs/commits/feature/noverlay
    (setq cquery-sem-highlight-method 'overlay)
    (cquery-use-default-rainbow-sem-highlight)
    (setq cquery-extra-init-params '(:cacheFormat "msgpack" :completion (:detailedLabel t)))

    (require 'projectile)
    (add-to-list 'projectile-globally-ignored-directories ".cquery_cached_index")
    ))

(defun cquery/post-init-company ()
  (spacemacs|add-company-backends :backends company-lsp :modes c-mode-common)
  )
