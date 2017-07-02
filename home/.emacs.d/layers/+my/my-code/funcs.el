(defun my-ffap ()
  (interactive)
  (let ((filename (ffap-guess-file-name-at-point)))
    (when (not filename)
      (user-error "No file at point"))
    (ffap filename)))

(defun my-find-tag ()
  (interactive)
  (let ((old-buffer (current-buffer))
        (old-point (point)))
    (if (and (boundp 'helm-kythe-mode)
             helm-kythe-mode)
        (helm-kythe-find-definitions)
      (helm-gtags-find-tag-from-here))
    (if (and (equal old-buffer (current-buffer))
             (equal old-point (point))
             (not (locate-dominating-file default-directory "GTAGS")))
        (evil-jump-to-tag))))

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
  "Jump to definition around point using the best tool for this action."
  (interactive)
  (catch 'done
    (let ((old-buffer (current-buffer))
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
                    (not (equal old-buffer (current-buffer))))
            (throw 'done t)))))
    (message "No reference handler was able to find this symbol.")))
