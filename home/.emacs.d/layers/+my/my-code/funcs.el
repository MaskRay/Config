(defun my/ffap ()
  (interactive)
  (let ((filename (ffap-guess-file-name-at-point)))
    (when (not filename)
      (user-error "No file at point"))
    (ffap filename)))

(defun my/find-tag ()
  (interactive)
  (let ((old-buffer (current-buffer))
        (old-point (point)))
    (helm-gtags-find-tag-from-here)
    (if (and (equal old-buffer (current-buffer))
             (equal old-point (point))
             (not (locate-dominating-file default-directory "GTAGS")))
        (evil-jump-to-tag))))


;;; realgud

(defun my/realgud-eval-nth-name-forward (n)
  (interactive "p")
  (save-excursion
    (let (name)
      (while (and (> n 0) (< (point) (point-max)))
        (let ((p (point)))
          (if (not (c-forward-name))
              (progn
                (c-forward-token-2)
                (when (= (point) p) (forward-char 1)))
            (setq name (buffer-substring-no-properties p (point)))
            (cl-decf n 1))))
      (when name
        (realgud:cmd-eval name)
        ))))

(defun my/realgud-eval-nth-name-backward (n)
  (interactive "p")
  (save-excursion
    (let (name)
      (while (and (> n 0) (> (point) (point-min)))
        (let ((p (point)))
          (c-backward-token-2)
          (when (= (point) p) (backward-char 1))
          (setq p (point))
          (when (c-forward-name)
            (setq name (buffer-substring-no-properties p (point)))
            (goto-char p)
            (cl-decf n 1))))
      (when name
        (realgud:cmd-eval name)
        ))))

(defun my/realgud-eval-region-or-word-at-point ()
  (interactive)
  (when-let
      ((cmdbuf (realgud-get-cmdbuf))
       (process (get-buffer-process cmdbuf))
       (expr
        (if (evil-visual-state-p)
            (let ((range (evil-visual-range)))
              (buffer-substring-no-properties (evil-range-beginning range)
                                              (evil-range-end range)))
          (word-at-point)
          )))
    (with-current-buffer cmdbuf
	    (setq realgud:process-filter-save (process-filter process))
	    (set-process-filter process 'realgud:eval-process-output))
    (realgud:cmd-eval expr)
    ))


;;; elisp

(defun my/realtime-elisp-doc-function ()
  (let ((w (selected-window)))
    (when-let (s (intern-soft (current-word)))
      (cond
       ((fboundp s) (describe-function s))
       ((boundp s) (describe-variable s))
       )
      (select-window w)
      nil)))

(defun my/realtime-elisp-doc ()
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
    (if (advice-function-member-p #'my/realtime-elisp-doc-function eldoc-documentation-function)
        (remove-function (local 'eldoc-documentation-function) #'my/realtime-elisp-doc-function)
      (add-function :after-while (local 'eldoc-documentation-function) #'my/realtime-elisp-doc-function))))


;;; xref

(defvar my-xref--jumps (make-hash-table)
  "Hashtable which stores all jumps on a per window basis.")

(defmacro my-xref//with-evil-jumps (&rest body)
  "Make `evil-jumps.el' commands work on `my-xref--jumps'."
  (declare (indent 1))
  `(let ((evil--jumps-window-jumps ,my-xref--jumps))
     ,@body
     ))

(with-eval-after-load 'evil-jumps
  (evil-define-motion my-xref/evil-jump-backward (count)
    (my-xref//with-evil-jumps
        (evil--jump-backward count)
      (run-hooks 'xref-after-return-hook)
    ))

  (evil-define-motion my-xref/evil-jump-forward (count)
    (my-xref//with-evil-jumps
        (evil--jump-forward count)
      (run-hooks 'xref-after-return-hook)
    )))

(defun my-xref/find-definitions ()
  (interactive)
  (when (eq xref-backend-functions 'lsp--xref-backend)
    (call-interactively 'xref-find-definitions)))

(defun my-xref/find-references ()
  (interactive)
  (when (eq xref-backend-functions 'lsp--xref-backend)
    (call-interactively 'xref-find-references)))

(defun my-xref/jump-backward ()
  (interactive)
  (pcase major-mode
    ((or c-mode c++-mode)
     (if lsp-mode
         (my-xref/evil-jump-backward)
         (helm-gtags-pop-stack)))
    (_ (helm-gtags-pop-stack))
    ))

(defun my-xref/jump-forward ()
  (interactive)
  (pcase major-mode
    ((or c-mode c++-mode)
     (if lsp-mode
         (my-xref/evil-jump-forward)
         (evil-jump-forward)))
    (_ (evil-jump-forward))
    ))

;;; Override
;; This function is transitively called by xref-find-definitions and xref-find-references
(require 'xref)
(defun xref--show-xrefs (xrefs display-action &optional always-show-list)
  (cond
   ((and (not (cdr xrefs)) (not always-show-list))
    ;; PATCH
    (my-xref//with-evil-jumps (evil-set-jump))

    (xref--pop-to-location (car xrefs) display-action))
   (t
    ;; PATCH
    (my-xref//with-evil-jumps (evil-set-jump))

    ;; PATCH Jump to the first candidate
    (when xrefs
      (xref--pop-to-location (car xrefs) display-action))

    (funcall xref-show-xrefs-function xrefs
             `((window . ,(selected-window)))))))

(defun my-advice/xref--show-xref-buffer (orig-fun &rest args)
  (save-selected-window
    (apply orig-fun args))
  )
(advice-add 'xref--show-xref-buffer :around #'my-advice/xref--show-xref-buffer)



;;; lsp-mode

(with-eval-after-load 'lsp-mode
  (defun lsp--location-to-xref (location &optional read)
    "Convert Location object LOCATION to an ‘xref-item’.
interface Location {
    uri: string;
    range: Range;
}"
    (lsp--send-changes lsp--cur-workspace)
    (let*
        ((orig-uri (gethash "uri" location))
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
      (xref-make summary (xref-make-file-location uri line column)))))


;; https://github.com/syl20bnr/spacemacs/pull/9911

(defmacro spacemacs|define-reference-handlers (mode &rest handlers)
  "Defines reference handlers for the given MODE.
This defines a variable `spacemacs-reference-handlers-MODE' to which
handlers can be added, and a function added to MODE-hook which
sets `spacemacs-reference-handlers' in buffers of that mode."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "spacemacs//init-reference-handlers-%S" mode)))
        (handlers-list (intern (format "spacemacs-reference-handlers-%S" mode))))
    `(progn
       (defvar ,handlers-list ',handlers
         ,(format (concat "List of mode-specific reference handlers for %S. "
                          "These take priority over those in "
                          "`spacemacs-default-reference-handlers'.")
                  mode))
       (defun ,func ()
         (setq spacemacs-reference-handlers
               (append ,handlers-list
                       spacemacs-default-reference-handlers)))
       (add-hook ',mode-hook ',func)
       (with-eval-after-load 'bind-map
         (spacemacs/set-leader-keys-for-major-mode ',mode
           "gr" 'spacemacs/jump-to-reference)))))

(defun spacemacs/jump-to-reference ()
  "Jump to reference around point using the best tool for this action."
  (interactive)
  (catch 'done
    (let ((old-window (selected-window))
          (old-buffer (current-buffer))
          (old-point (point)))
      (dolist (-handler spacemacs-reference-handlers)
        (let ((handler (if (listp -handler) (car -handler) -handler))
              (async (when (listp -handler)
                       (plist-get (cdr -handler) :async))))
          (ignore-errors
            (call-interactively handler))
          (when (or (eq async t)
                    (and (fboundp async) (funcall async))
                    (not (eq old-point (point)))
                    (not (equal old-buffer (window-buffer old-window))))
            (throw 'done t)))))
    (message "No reference handler was able to find this symbol.")))

(defun spacemacs/jump-to-reference-other-window ()
  "Jump to reference around point in other window."
  (interactive)
  (let ((pos (point)))
    ;; since `spacemacs/jump-to-reference' can be asynchronous we cannot use
    ;; `save-excursion' here, so we have to bear with the jumpy behavior.
    (switch-to-buffer-other-window (current-buffer))
    (goto-char pos)
    (spacemacs/jump-to-reference)))
