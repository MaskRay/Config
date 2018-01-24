(defconst lsp-packages
  '(
    (company-lsp :requires company)
    (helm-xref :requires helm)
    (ivy-xref :requires ivy)
    (lsp-mode :location local)
    (lsp-ui :location local)
    lsp-python
    ))

(defun lsp/init-company-lsp ()
  (use-package company-lsp
    :defer t
    :init
    (setq company-quickhelp-delay 0)
    ;; Language servers have better idea filtering and sorting,
    ;; don't filter results on the client side.
    (setq company-transformers nil
          company-lsp-async t
          company-lsp-cache-candidates nil)
    (spacemacs|add-company-backends :backends company-lsp :modes c-mode-common)
    ))

(defun lsp/init-helm-xref ()
  (use-package helm-xref
    :config
    (progn
      ;; This is required to make xref-find-references not give a prompt.
      ;; xref-find-references asks the identifier (which has no text property) and then passes it to lsp-mode, which requires the text property at point to locate the references.
      ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=29619
      (setq xref-prompt-for-identifier
            '(not xref-find-definitions xref-find-definitions-other-window xref-find-definitions-other-frame xref-find-references spacemacs/jump-to-definition))

      ;; Use helm-xref to display xref.el results.
      (setq xref-show-xrefs-function #'helm-xref-show-xrefs)
      )))

(defun lsp/init-ivy-xref ()
  (use-package ivy-xref
    :config
    (progn
      (setq xref-prompt-for-identifier
            '(not xref-find-definitions xref-find-definitions-other-window xref-find-definitions-other-frame xref-find-references spacemacs/jump-to-definition))

      ;; Use ivy-xref to display xref.el results.
      (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
      )))

(defun lsp/init-lsp-mode ()
  (use-package lsp-mode
    :config
    (progn
      (require 'lsp-imenu)
      (add-hook 'lsp-after-open-hook #'lsp-enable-imenu)
      ;; Disable lsp-flycheck.el in favor of lsp-ui-flycheck.el
      (setq lsp-enable-flycheck nil)
      (spacemacs|diminish lsp-mode " Ⓛ" " L")
      )))

(defun lsp/init-lsp-ui ()
  (use-package lsp-ui
    :after lsp-mode
    :after markdown-mode
    :config
    (progn
      (add-hook 'lsp-mode-hook #'lsp-ui-mode)

      (lsp//sync-peek-face)
      (add-hook 'spacemacs-post-theme-change-hook #'lsp//sync-peek-face)
      )))

(defun lsp/init-lsp-python ()
  (use-package lsp-python
    :init
    (add-hook 'python-mode-hook #'lsp-python-enable)
    ))
