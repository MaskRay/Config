(require 'cl-lib)
(require 'subr-x)

(defun my//enable-cquery-if-compile-commands-json ()
  (when
      (and (not (and (boundp 'lsp-mode) lsp-mode))
           (or
            (cl-some (lambda (x) (string-match-p x buffer-file-name)) my-cquery-whitelist)
            (cl-notany (lambda (x) (string-match-p x buffer-file-name)) my-cquery-blacklist))
           (or (locate-dominating-file default-directory "compile_commands.json")
               (locate-dominating-file default-directory ".cquery")))
    (setq eldoc-idle-delay 0.2)
    (lsp-cquery-enable)
    (lsp-enable-imenu)))


;; xref-find-apropos (workspace/symbol)

(defun my/highlight-pattern-in-text (pattern line)
  (when (> (length pattern) 0)
    (let ((i 0))
     (while (string-match pattern line i) ;
       (setq i (match-end 0))
       (add-face-text-property (match-beginning 0) (match-end 0) 'highlight t line)
       )
     line)))

(with-eval-after-load 'lsp-methods
  ;;; Override
  ;; Use containerName instead of name if available
  (defun lsp--symbol-information-to-xref (symbol)
   "Return a `xref-item' from SYMBOL information."
   (let* ((location (gethash "location" symbol))
          (uri (gethash "uri" location))
          (range (gethash "range" location))
          (start (gethash "start" range))
          (name (gethash "name" symbol))
          (container-name (gethash "containerName" symbol)))
     (xref-make (format "[%s] %s"
                        (alist-get (gethash "kind" symbol) lsp--symbol-kind)
                        (if container-name
                            (my/highlight-pattern-in-text (regexp-quote name) container-name)
                          name))
                (xref-make-file-location (string-remove-prefix "file://" uri)
                                         (1+ (gethash "line" start))
                                         (gethash "character" start))))))

(defun cquery-vars ()
  (interactive)
  (xref--show-xrefs
   (lsp--locations-to-xref-items
    (lsp--send-request (lsp--make-request "$cquery/vars"
                                          (lsp--make-reference-params)))) nil))
