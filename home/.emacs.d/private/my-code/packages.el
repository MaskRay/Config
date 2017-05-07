(defconst my-code-packages
  '(my-rtags))

(defun my-code/init-my-rtags ()
  (define-key evil-normal-state-map (kbd "C-,") 'spacemacs/jump-to-reference)
  )

(defun my-code/post-init-my-rtags ()
  (add-to-list 'spacemacs-reference-handlers-c++-mode 'rtags-find-references-at-point)
  (add-to-list 'spacemacs-reference-handlers-c-mode 'rtags-find-references-at-point)
  )

;;; packages.el ends here
