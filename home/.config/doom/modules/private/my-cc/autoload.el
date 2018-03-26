;;; private/my-cc/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +cquery//enable ()
  (let ((filename (buffer-file-name)))
    (when (-none? (lambda (x) (string-match-p x filename)) +my/cquery-blacklist))
    (condition-case nil
        (lsp-cquery-enable)
      (user-error nil))))

(defun cquery/base () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/base"))
(defun cquery/callers () (interactive) (lsp-ui-peek-find-custom 'callers "$cquery/callers"))
(defun cquery/derived () (interactive) (lsp-ui-peek-find-custom 'derived "$cquery/derived"))
(defun cquery/vars () (interactive) (lsp-ui-peek-find-custom 'vars "$cquery/vars"))
(defun cquery/random () (interactive) (lsp-ui-peek-find-custom 'random "$cquery/random"))
(defun text-document/type-definition () (interactive) (lsp-ui-peek-find-custom 'type "textDocument/typeDefinition"))

(defun cquery/references-address ()
  (interactive)
  (lsp-ui-peek-find-custom
   'address "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context
              '(:role 128))))

(defun cquery/references-read ()
  (interactive)
  (lsp-ui-peek-find-custom
   'read "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context
              '(:role 8))))

(defun cquery/references-write ()
  (interactive)
  (lsp-ui-peek-find-custom
   'write "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context
              '(:role 16))))

;; xref-find-apropos (workspace/symbol)

(defun my/highlight-pattern-in-text (pattern line)
  (when (> (length pattern) 0)
    (let ((i 0))
     (while (string-match pattern line i)
       (setq i (match-end 0))
       (add-face-text-property (match-beginning 0) (match-end 0) 'isearch t line)
       )
     line)))

(with-eval-after-load 'lsp-methods
  ;;; Override
  ;; This deviated from the original in that it highlights pattern appeared in symbol
  (defun lsp--symbol-information-to-xref (pattern symbol)
   "Return a `xref-item' from SYMBOL information."
   (let* ((location (gethash "location" symbol))
          (uri (gethash "uri" location))
          (range (gethash "range" location))
          (start (gethash "start" range))
          (name (gethash "name" symbol)))
     (xref-make (format "[%s] %s"
                        (alist-get (gethash "kind" symbol) lsp--symbol-kind)
                        (my/highlight-pattern-in-text (regexp-quote pattern) name))
                (xref-make-file-location (string-remove-prefix "file://" uri)
                                         (1+ (gethash "line" start))
                                         (gethash "character" start)))))

  (cl-defmethod xref-backend-apropos ((_backend (eql xref-lsp)) pattern)
    (let ((symbols (lsp--send-request (lsp--make-request
                                       "workspace/symbol"
                                       `(:query ,pattern)))))
      (mapcar (lambda (x) (lsp--symbol-information-to-xref pattern x)) symbols)))
  )
