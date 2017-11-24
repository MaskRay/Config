(require 'cl-lib)
(require 'subr-x)

(defun my//enable-cquery-if-compile-commands-json ()
  (let ((filename (buffer-file-name)))
    (if-let ((_ (not (and (boundp 'lsp-mode) lsp-mode)))
             (_ (cl-every (lambda (x) (not (string-match-p x filename))) my-cquery-blacklist))
             (root (projectile-project-root))
             (_ (or (file-exists-p (concat root "compile_commands.json"))
                    (file-exists-p (concat root "clang_args")))))
        (lsp-cquery-enable))))

;;; Override
(defun lsp--location-to-xref (location &optional read)
  "Convert Location object LOCATION to an ‘xref-item’.
interface Location {
    uri: string;
    range: Range;
}"
  (lsp--send-changes lsp--cur-workspace)
  (let* ((orig-uri (gethash "uri" location))
         (uri (string-remove-prefix "file://" orig-uri))
         (ref-pos (gethash "start" (gethash "range" location)))
         (line (1+ (gethash "line" ref-pos)))
         (column (gethash "character" ref-pos))
         (summary (if (string-prefix-p "file://" orig-uri)
                      (let ((s (if-let ((buf (find-buffer-visiting uri)))
                                   (with-current-buffer buf
                                     (save-excursion
                                       (goto-char (point-min))
                                       (forward-line (1- line))
                                       (buffer-substring (line-beginning-position) (line-end-position))))
                                 (with-temp-buffer
                                   (insert-file-contents uri nil)
                                   (goto-char (point-min))
                                   (forward-line (1- line))
                                   (buffer-substring (line-beginning-position) (line-end-position))))))
                        (add-face-text-property column (+ column (length (word-at-point))) 'highlight t s)
                        s
                        )
                    uri)))
    (xref-make summary (xref-make-file-location uri line column))))
