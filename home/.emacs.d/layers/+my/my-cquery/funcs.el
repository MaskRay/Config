(require 'cl-lib)
(require 'subr-x)

(defun my//enable-cquery-if-compile-commands-json ()
  (let ((filename (buffer-file-name)))
    (when-let
        ((_ (not (and (boundp 'lsp-mode) lsp-mode)))
         (_ (cl-every (lambda (x) (not (string-match-p x filename))) my-cquery-blacklist))
         (root (projectile-project-root))
         (_ (or (file-exists-p (concat root "compile_commands.json"))
                (file-exists-p (concat root "clang_args")))))
      (lsp-cquery-enable)
      (lsp-enable-imenu))))

(defun lsp--get-workspace-symbols ()
  (lsp--cur-workspace-check)
  (lsp--send-request (lsp--make-request
                      "workspace/symbol"
                      `(:textDocument ,(lsp--text-document-identifier)))))

(defun my/lsp--imenu-create-index ()
  (let ((symbols (seq-remove #'lsp--symbol-filter (lsp--get-workspace-symbols))))
    (mapcar (lambda (nested-alist)
              (cons (car nested-alist)
                    (mapcar #'lsp--symbol-to-imenu-elem (cdr nested-alist))))
            (seq-group-by #'lsp--get-symbol-type symbols))))
