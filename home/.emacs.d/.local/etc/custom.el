;;;  -*- lexical-binding: t; -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(async-shell-command-display-buffer nil)
 '(c-backslash-max-column 80)
 '(counsel-grep-base-command
   "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
 '(evil-want-Y-yank-to-eol nil)
 '(ispell-program-name "/usr/bin/hunspell")
 '(paradox-github-token t t)
 '(realgud-safe-mode nil)
 '(x86-lookup-browse-pdf-function 'x86-lookup-browse-pdf-evince)
 '(x86-lookup-pdf "~/Documents/x86/325383-sdm-vol-2abcd.pdf"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cquery-sem-global-variable-face ((t (:underline t :weight extra-bold))))
 '(lsp-face-highlight-read ((t (:distant-foreground nil :foreground nil :background "#234011"))))
 '(lsp-face-highlight-textual ((t (:distant-foreground nil :foreground nil :background "gray25"))))
 '(lsp-face-highlight-write ((t (:distant-foreground nil :foreground nil :background "#402311"))))
 '(lsp-ui-sideline-current-symbol ((t (:foreground "grey38" :box nil))))
 '(lsp-ui-sideline-symbol ((t (:foreground "grey30" :box nil)))))
