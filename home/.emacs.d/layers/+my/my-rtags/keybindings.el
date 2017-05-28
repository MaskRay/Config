;; (defconst my-rtags-overrides
;;   '(("C-j" 'rtags-find-symbol-at-point)
;;     ("C-," 'rtags-find-references-at-point)))
;; 
;; (defun my-rtags-set-evil-keys ()
;;   (dolist (override my-rtags-overrides)
;;     (evil-local-set-key 'normal (car override) (cdr override))))
;; 
;; (add-hook 'c-mode-common-hook 'my-rtags-set-evil-keys)

;; (evil-define-key 'normal c++-mode-map (kbd "C-j") 'rtags-find-symbol-at-point)

(dolist (mode '(c-mode c++-mode))
  (evil-leader/set-key-for-mode mode
    "g ." 'rtags-find-symbol-at-point
    "g ," 'rtags-find-references-at-point
    "g v" 'rtags-find-virtuals-at-point
    "g V" 'rtags-print-enum-value-at-point
    "g /" 'rtags-find-all-references-at-point
    "g Y" 'rtags-cycle-overlays-on-screen
    "g >" 'rtags-find-symbol
    "g <" 'rtags-find-references
    "g [" 'rtags-location-stack-back
    "g ]" 'rtags-location-stack-forward
    "g D" 'rtags-diagnostics
    "g G" 'rtags-guess-function-at-point
    "g p" 'rtags-set-current-project
    "g P" 'rtags-print-dependencies
    "g e" 'rtags-reparse-file
    "g E" 'rtags-preprocess-file
    "g M" 'rtags-symbol-info
    "g S" 'rtags-display-summary
    "g O" 'rtags-goto-offset
    "g ;" 'rtags-find-file
    "g F" 'rtags-fixit
    "g L" 'rtags-copy-and-print-current-location
    "g X" 'rtags-fix-fixit-at-point
    "g B" 'rtags-show-rtags-buffer
    "g I" 'rtags-imenu
    "g T" 'rtags-taglist
    "g h" 'rtags-print-class-hierarchy
    "g a" 'rtags-print-source-arguments))
