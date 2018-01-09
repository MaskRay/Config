(defconst my-cquery-packages '(lsp-mode))

;; TODO Rename to my-cquery/init-cquery after cquery.el is integrated into MELPA
(defun my-cquery/post-init-lsp-mode ()
  (add-to-load-path (expand-file-name "~/Dev/Util/cquery/emacs"))
  (require 'lsp-imenu)
  (use-package cquery
    :after lsp-mode
    :config
    ;; overlay is slow
    ;; WAIT https://lists.gnu.org/archive/html/emacs-devel/2017-05/msg00084.html
    (setq cquery-sem-highlight-method 'font-lock)
    ;; cquery.cl cquery--publich-semantic-highlighting is very slow
    (setq cquery-enable-sem-highlight nil)
    (setq cquery-extra-init-params '(:enableComments 2 :cacheFormat "msgpack"))
    (add-hook 'c-mode-common-hook #'my//enable-cquery-if-compile-commands-json))

  (with-eval-after-load 'helm-imenu
    ;;; Override
    ;; Revert removing *Rescan*
    (defun helm-imenu-candidates (&optional buffer)
     (with-current-buffer (or buffer helm-current-buffer)
       (let ((tick (buffer-modified-tick)))
         (if (eq helm-cached-imenu-tick tick)
             helm-cached-imenu-candidates
           (setq imenu--index-alist nil)
           (prog1 (setq helm-cached-imenu-candidates
                        (let ((index (imenu--make-index-alist t)))
                          (helm-imenu--candidates-1 index)))
             (setq helm-cached-imenu-tick tick))))))
    ))
