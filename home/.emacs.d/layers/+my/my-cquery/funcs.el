(defun my//enable-cquery-if-compile-commands-json ()
  (let ((filename (buffer-file-name)))
    (when (-none? (lambda (x) (s-matches? x filename)) my-cquery-blacklist))
    (lsp-cquery-enable)))
