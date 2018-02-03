(defconst cquery-packages '((cquery :location local)))

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
    ))
