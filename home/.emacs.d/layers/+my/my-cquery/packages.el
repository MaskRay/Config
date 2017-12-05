(defconst my-cquery-packages '(lsp-mode))

;; TODO Rename to my-cquery/init-cquery after cquery.el is integrated into MELPA
(defun my-cquery/post-init-lsp-mode ()
  (add-to-load-path (expand-file-name "~/Dev/Util/cquery/emacs"))
  (require 'lsp-imenu)
  (use-package cquery
    :after lsp-mode
    :config
    (setq cquery-resource-dir (expand-file-name "~/Dev/Util/cquery/clang_resource_dir/"))
    (add-hook 'c-mode-common-hook #'my//enable-cquery-if-compile-commands-json))
  (spacemacs/set-leader-keys
    "jl" (lambda ()
           (interactive)
           (let ((imenu--index-alist nil)
                 (imenu-create-index-function #'my/lsp--imenu-create-index))
             (call-interactively #'imenu)))
    ))
