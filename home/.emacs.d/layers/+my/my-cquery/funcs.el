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


;; workspace/symbol

;; lsp-methods.el specializes xref-find-apropos, which lists all
;; workspace/symbol. However, xref-location-marker is called for each candidate,
;; which is very slow when there are many symbols. I define a variant of
;; xref-find-apropos which lazily calls xref-location-marker when the candidate
;; is selected.

;; (defun lsp--get-workspace-symbols ()
;;   (lsp--cur-workspace-check)
;;   (lsp--send-request (lsp--make-request
;;                       "workspace/symbol"
;;                       `(:textDocument ,(lsp--text-document-identifier)))))
;;

(defun my/highlight-pattern-in-text (pattern line)
  (when (> (length pattern) 0)
    (let ((i 0))
     (while (string-match pattern line i) ;
       (setq i (match-end 0))
       (add-face-text-property (match-beginning 0) (match-end 0) 'highlight t line)
       )
     line)))

;; Override
;;

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

;; Adapted from helm-xref-goto-location
(defun my-xref/helm-xref-goto-location (candidate func)
  (-when-let ((_ . xref) (assoc candidate my-helm-xref-alist))
    (with-slots (summary location) xref
      (let ((marker (xref-location-marker location)))
        (helm-xref-goto-location marker func)))))

;; Adapted from helm-xref-source
(defun my-xref/helm-xref-source ()
  (helm-build-sync-source "no-find-file Helm Xref"
    :candidates (lambda () (reverse my-helm-xref-alist))
    :persistent-action (lambda (candidate)
                         (my-xref/helm-xref-goto-location candidate 'display-buffer))
    :action (lambda (candidate)
              (my-xref/helm-xref-goto-location candidate 'switch-to-buffer))
    :candidate-transformer
    (lambda (candidates)
      (mapcar #'car candidates))
    :candidate-number-limit 999))

(defvar my-helm-xref-alist nil)

;; Adapted from helm-xref-show-xrefs
(defun my-xref/helm-show-symbols (xrefs)
  (setq my-helm-xref-alist nil)
  (dolist (xref xrefs)
    (with-slots (summary location) xref
      (let* ((line (xref-location-line location))
             (file (xref-location-group location))
             candidate)
        (setq candidate
              (concat
               (propertize (car (reverse (split-string file "\\/")))
                           'font-lock-face 'helm-xref-file-name)
               (when (string= "integer" (type-of line))
                 (concat
                  ":"
                  (propertize (int-to-string line)
                              'font-lock-face 'helm-xref-line-number)))
               ":"
               summary))
        (push (cons candidate xref) my-helm-xref-alist))))
  (helm :sources (my-xref/helm-xref-source)
        :truncate-lines t
        :buffer "*helm-workspace-symbol*"))

(defun my-xref/find-apropos (pattern)
  (interactive (list (read-string
                      "Search for workspace/symbol: "
                      nil 'xref--read-pattern-history)))
  (let ((symbols (xref-backend-apropos (lsp--xref-backend) pattern)))
    (unless symbols
      (user-error "No symbol found for: %s" pattern))
    (my-xref//with-evil-jumps (evil-set-jump))
    (my-xref/helm-show-symbols symbols)))
