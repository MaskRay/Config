(require 'cl-lib)

(defun my//enable-cquery-if-compile-commands-json ()
  (let ((filename (buffer-file-name)))
    (if-let* ((_ (cl-every (lambda (x) (not (string-match-p x filename))) my-cquery-blacklist))
              (root (projectile-project-root))
              (_ (or (file-exists-p (concat root "compile_commands.json"))
                   (file-exists-p (concat root "clang_args")))))
        (lsp-cquery-enable))))
