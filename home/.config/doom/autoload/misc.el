;;; private/my/autoload/misc.el -*- lexical-binding: t; -*-

;; PATCH counsel-esh-history
;;;###autoload
(defun +my/ivy-eshell-history ()
  (interactive)
  (require 'em-hist)
  (let* ((start-pos (save-excursion (eshell-bol) (point)))
         (end-pos (point))
         (input (buffer-substring-no-properties start-pos end-pos))
         (command (ivy-read "Command: "
                            (delete-dups
                             (when (> (ring-size eshell-history-ring) 0)
                               (ring-elements eshell-history-ring)))
                            :initial-input input)))
    (setf (buffer-substring start-pos end-pos) command)
    (end-of-line)))

;;;###autoload
(defun +my/ffap ()
  (interactive)
  (-if-let (filename (ffap-guess-file-name-at-point))
      (find-file filename)
    (user-error "No file at point")))

(defun +my/avy-goto-paren ()
  (interactive)
  (avy--generic-jump "(" nil 'pre))

(defun +my/avy-goto-conditional ()
  (interactive)
  (avy--generic-jump "\\s(\\(if\\|cond\\|when\\|unless\\)\\b" nil 'pre))

(defun my/define-key (keymap key def &rest bindings)
  "Define multi keybind with KEYMAP KEY DEF BINDINGS."
  (interactive)
  (while key
    (define-key keymap (kbd key) def)
    (setq key (pop bindings)
          def (pop bindings))))

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
        nil))))

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
        nil))))

(defun my/realgud-eval-region-or-word-at-point ()
  (interactive)
  (when-let*
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

(defun +my//realtime-elisp-doc-function ()
  (-when-let* ((w (selected-window))
               (s (intern-soft (current-word))))
    (describe-symbol s)
    (select-window w)))

;;;###autoload
(defun +my/realtime-elisp-doc ()
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
    (if (advice-function-member-p #'+my//realtime-elisp-doc-function eldoc-documentation-function)
        (remove-function (local 'eldoc-documentation-function) #'+my//realtime-elisp-doc-function)
      (add-function :after-while (local 'eldoc-documentation-function) #'+my//realtime-elisp-doc-function))))

;;;###autoload
(defun +my/find-definitions ()
  (interactive)
  (if lsp-mode (lsp-ui-peek-find-definitions) (call-interactively #'+lookup/definition)))

;;;###autoload
(defun +my/find-references ()
  (interactive)
  (if lsp-mode (lsp-ui-peek-find-references) (call-interactively #'+lookup/references)))

(defmacro +my//xref-jump-file (command)
  `(let* ((target (buffer-name))
          (last target) (last-point (point))
          (curr target) (curr-point (point)))
     (cl-loop do
              ,command
              (setq curr (buffer-name) curr-point (point))
              until (or (string= target curr)
                        (and (string= last curr) (= last-point curr-point))
                        (prog1 nil (setq last curr last-point curr-point))
                        ))))

;;;###autoload
(defun +my/xref-jump-backward-file ()
  (interactive)
  (+my//xref-jump-file (lsp-ui-peek-jump-backward)))

;;;###autoload
(defun +my/xref-jump-forward-file ()
  (interactive)
  (+my//xref-jump-file (lsp-ui-peek-jump-forward)))

;;;###autoload
(defun +my/avy-document-symbol (all)
  (interactive)
  (let ((line 0) (col 0) (w (selected-window))
        (ccls (memq major-mode '(c-mode c++-mode objc-mode)))
        (start-line (1- (line-number-at-pos (window-start))))
        (end-line (1- (line-number-at-pos (window-end))))
        ranges point0 point1
        candidates)
    (save-excursion
      (goto-char 1)
      (cl-loop for loc in
               (lsp--send-request (lsp--make-request
                                   "textDocument/documentSymbol"
                                   `(:textDocument ,(lsp--text-document-identifier)
                                                   :all ,(if all t :json-false)
                                                   :startLine ,start-line :endLine ,end-line)))
               for range = (if ccls loc (->> loc (gethash "location") (gethash "range")))
               for range_start = (gethash "start" range)
               for range_end = (gethash "end" range)
               for l0 = (gethash "line" range_start)
               for c0 = (gethash "character" range_start)
               for l1 = (gethash "line" range_end)
               for c1 = (gethash "character" range_end)
               while (<= l0 end-line)
               when (>= l0 start-line)
               do
               (forward-line (- l0 line))
               (forward-char c0)
               (setq point0 (point))
               (forward-line (- l1 l0))
               (forward-char c1)
               (setq point1 (point))
               (setq line l1 col c1)
               (push `((,point0 . ,point1) . ,w) candidates)))
    (avy-with avy-document-symbol
      (avy--process candidates
                    (avy--style-fn avy-style)))))

;;;###autoload
(defun +my/lsp-ui-doc--eldoc (&rest _)
  (when (and (lsp--capability "documentHighlightProvider")
             lsp-highlight-symbol-at-point)
    (lsp-symbol-highlight))
  (if (evil-insert-state-p)
      (lsp--text-document-signature-help)
      lsp-ui-doc--string-eldoc
      ))

;;;###autoload
(defun counsel-imenu-comments ()
  "Imenu display comments."
  (interactive)
  (require 'evil-nerd-commenter)
  (let* ((imenu-create-index-function 'evilnc-imenu-create-index-function))
    (counsel-imenu)))

;;;###autoload
(defun +my/realgud-eval-nth-name-forward (n)
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
        (realgud:cmd-eval name)))))

;;;###autoload
(defun +my/realgud-eval-nth-name-backward (n)
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
        (realgud:cmd-eval name)))))
