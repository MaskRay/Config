;; -*- mode: emacs-lisp -*-
;; This file is where Emacs writes custom variables.
;; Spacemacs will copy its content to your dotfile automatically in the
;; function `dotspacemacs/emacs-custom-settings'.
;; Do not alter this file, use Emacs customize interface instead.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-backslash-max-column 80)
 '(evil-want-Y-yank-to-eol nil)
 '(gtags-enable-by-default nil t)
 '(haskell-completing-read-function (quote helm--completing-read-default))
 '(ispell-program-name "/usr/bin/hunspell")
 '(package-selected-packages
   (quote
    (pipenv x86-lookup nasm-mode pdf-tools tablist el-search treemacs-projectile treemacs-evil treemacs pfuture company-lsp zeal-at-point yapfify yaml-mode xterm-color ws-butler winum which-key web-mode web-beautify volatile-highlights vimrc-mode vi-tilde-fringe uuidgen use-package unfill toc-org tagedit symon string-inflection spaceline powerline smeargle slim-mode shm shell-pop scss-mode sass-mode restclient-helm restart-emacs realgud test-simple loc-changes load-relative rainbow-mode rainbow-identifiers rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode pip-requirements persp-mode pcre2el password-generator paradox spinner orgit org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-download org-bullets org-brain open-junk-file ob-restclient ob-http neotree mwim multi-term move-text mmm-mode markdown-toc markdown-mode magit-gitflow macrostep lsp-rust rust-mode lsp-haskell lsp-mode lorem-ipsum llvm-mode livid-mode skewer-mode live-py-mode linum-relative link-hint less-css-mode json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc intero indent-guide impatient-mode simple-httpd idris-mode prop-menu ibuffer-projectile hydra hy-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-xref helm-themes helm-swoop helm-pydoc helm-purpose window-purpose imenu-list helm-projectile helm-mode-manager helm-make projectile helm-hoogle helm-gtags helm-gitignore helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag haskell-snippets haml-mode google-translate golden-ratio godoctor go-rename go-guru go-eldoc gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gh-md ggtags fuzzy flycheck-ycmd flycheck-pos-tip pos-tip flycheck-haskell flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-snipe evil-search-highlight-persist evil-org evil-numbers evil-mc evil-matchit evil-magit magit magit-popup git-commit with-editor evil-lisp-state smartparens evil-lion evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-commentary evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight eshell-z eshell-prompt-extras esh-help engine-mode emojify ht emoji-cheat-sheet-plus emmet-mode elisp-slime-nav editorconfig dumb-jump disaster diminish diff-hl define-word dante dactyl-mode d-mode cython-mode company-ycmd ycmd request-deferred let-alist request deferred company-web web-completion-data company-tern dash-functional tern company-statistics company-restclient restclient know-your-http-well company-go go-mode company-ghci company-ghc ghc haskell-mode company-emoji company-dcd ivy popwin flycheck-dmd-dub flycheck pkg-info epl company-cabal company-c-headers company-auctex company-anaconda company column-enforce-mode color-identifiers-mode coffee-mode cmm-mode cmake-mode clean-aindent-mode clang-format browse-at-remote bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol auto-compile packed auctex-latexmk auctex anaconda-mode pythonic f dash s aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup)))
 '(paradox-github-token t t)
 '(realgud-safe-mode nil)
 '(sp-autodelete-pair nil)
 '(sp-autoinsert-pair nil)
 '(sp-autoskip-closing-pair nil)
 '(tramp-syntax (quote default) nil (tramp))
 '(x86-lookup-browse-pdf-function (quote x86-lookup-browse-pdf-evince))
 '(x86-lookup-pdf "~/Documents/x86/325383-sdm-vol-2abcd.pdf"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-face-highlight-read ((t (:background "#234011"))))
 '(lsp-face-highlight-textual ((t (:background "gray25"))))
 '(lsp-face-highlight-write ((t (:background "#402311")))))
