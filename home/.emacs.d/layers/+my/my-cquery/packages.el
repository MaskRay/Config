(defconst my-cquery-packages '(lsp-mode))

;; TODO Rename to my-cquery/init-cquery after cquery.el is integrated into MELPA
(defun my-cquery/post-init-lsp-mode ()
  (add-to-load-path (expand-file-name "~/Dev/Util/cquery/emacs"))
  (require 'lsp-imenu)
  (use-package cquery
    :after lsp-mode
    :config
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

    ;;; Override
    ;; No (user-error "No word list given") if pattern is empty
    (defun xref-find-apropos (pattern)
      "Find all meaningful symbols that match PATTERN.
The argument has the same meaning as in `apropos'."
      (interactive (list (read-string
                          "Search for pattern (word list or regexp): "
                          nil 'xref--read-pattern-history)))
      (require 'apropos)
      (xref--find-xrefs pattern 'apropos pattern nil))
    )
  )
