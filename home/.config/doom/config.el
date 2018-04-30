;;; private/my/config.el -*- lexical-binding: t; -*-

(load! +bindings)
(load! +org)
(load! +ui)

(def-package! avy
  :commands (avy-goto-char-timer)
  :init
  (setq avy-timeout-seconds 0.2)
  )

(after! company
  (setq company-minimum-prefix-length 2
        company-quickhelp-delay nil
        company-show-numbers t
        company-backends '(company-capf company-dabbrev)
        company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode)
        ))

(set! :lookup 'emacs-lisp-mode :documentation #'helpful-at-point)

(after! eshell
  (defun eshell/l (&rest args) (eshell/ls "-l" args))
  (defun eshell/e (file) (find-file file))
  (defun eshell/md (dir) (eshell/mkdir dir) (eshell/cd dir))
  (defun eshell/ft (&optional arg) (treemacs arg))

  (defun eshell/up (&optional pattern)
    (let ((p (locate-dominating-file
              (f-parent default-directory)
              (lambda (p)
                (if pattern
                    (string-match-p pattern (f-base p))
                  t)))
             ))
      (eshell/pushd p)))
  )

(def-package! eshell-autojump)

(def-package! evil-nerd-commenter
  :commands (evilnc-comment-or-uncomment-lines)
  )

(after! evil-snipe
  (setq evil-snipe-scope 'buffer)
  )

(after! git-link
  (defun git-link-llvm (hostname dirname filename branch commit start end)
      (format "https://github.com/llvm-mirror/%s/tree/%s/%s"
              (file-name-base dirname)
                (or branch commit)
              (concat filename
                      (when start
                        (concat "#"
                                (if end
                                    (format "L%s-%s" start end)
                                  (format "L%s" start)))))))
    (add-to-list 'git-link-remote-alist '("git.llvm.org" git-link-llvm))
  )

(def-package! lispy
  :hook (emacs-lisp-mode . lispy-mode)
  :config
  (setq lispy-outline "^;; \\(?:;[^#]\\|\\*+\\)"
        lispy-outline-header ";; ")
  (map! :map lispy-mode-map
        :i "C-c (" #'lispy-wrap-round
        :i "_" #'special-lispy-different
        "d" nil
        :i [remap delete-backward-char] #'lispy-delete-backward))

;; Also use lispyville in prog-mode for [ ] < >
(def-package! lispyville
  :demand t
  :after (evil)
  :hook (lispy-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme
   '(operators
     c-w
     (escape insert)
     (slurp/barf-lispy)
     additional-movement))
  (map! :map emacs-lisp-mode-map
        :nm "gh" #'lispyville-left
        :nm "gl" #'lispyville-right
        :nm "J" #'lispyville-forward-sexp
        :nm "K" #'lispyville-backward-sexp
        :n "gh" #'+lookup/documentation
        :n "C-<left>" #'lispy-forward-barf-sexp
        :n "C-<right>" #'lispy-forward-slurp-sexp
        :n "C-M-<left>" #'lispy-backward-slurp-sexp
        :n "C-M-<right>" #'lispy-backward-barf-sexp
        :n "TAB" #'lispyville-prettify
        :localleader
        :n "e" (λ! (save-excursion (forward-sexp) (eval-last-sexp nil)))
        )
  )

(def-package! lsp-mode
  :defer t
  :init
  (setq lsp-project-blacklist '("/CC/"))
  )

(def-package! lsp-ui
  :demand t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq
   lsp-ui-sideline-enable nil
   lsp-ui-doc-header nil
   lsp-ui-doc-include-signature nil
   lsp-ui-doc-background (doom-color 'base4)
   lsp-ui-doc-border (doom-color 'fg)

   lsp-ui-peek-expand-function (lambda (xs) (mapcar #'car xs)))

  (map! :map lsp-ui-mode-map
        :localleader
        :n "lA" #'lsp-ui-peek-find-workspace-symbol
        :n "lF" #'lsp-format-buffer
        :n "ll" #'lsp-ui-sideline-mode
        :n "ld" #'lsp-ui-doc-mode
        :n "lr" #'lsp-rename

        :n "lp" #'ccls-preprocess-file
        :n "lf" #'ccls-freshen-index
        :n "a" #'ccls/references-address
        :n "r" #'ccls/references-read
        :n "w" #'ccls/references-write
        )

   (map! :after lsp-ui-peek
         :map lsp-ui-peek-mode-map
         "h" #'lsp-ui-peek--select-prev-file
         "j" #'lsp-ui-peek--select-next
         "k" #'lsp-ui-peek--select-prev
         "l" #'lsp-ui-peek--select-next-file
         )

   (defhydra hydra/ref (evil-normal-state-map "x")
     "reference"
     ("d" lsp-ui-peek-find-definitions "next" :bind nil)
     ("j" (-let [(i . n) (lsp-ui-find-next-reference)]
            (if (> n 0) (message "%d/%d" i n))) "next")
     ("k" (-let [(i . n) (lsp-ui-find-prev-reference)]
            (if (> n 0) (message "%d/%d" i n))) "prev")
     ("R" (-let [(i . n) (lsp-ui-find-prev-reference
                          (lambda (x)
                            (/= (logand (gethash "role" x 0) 8) 0)))]
            (if (> n 0) (message "read %d/%d" i n))) "prev read" :bind nil)
     ("r" (-let [(i . n) (lsp-ui-find-next-reference
                          (lambda (x)
                            (/= (logand (gethash "role" x 0) 8) 0)))]
            (if (> n 0) (message "read %d/%d" i n))) "next read" :bind nil)
     ("W" (-let [(i . n) (lsp-ui-find-prev-reference
                          (lambda (x)
                            (/= (logand (gethash "role" x 0) 16) 0)))]
            (if (> n 0) (message "write %d/%d" i n))) "prev write" :bind nil)
     ("w" (-let [(i . n) (lsp-ui-find-next-reference
                          (lambda (x)
                            (/= (logand (gethash "role" x 0) 16) 0)))]
            (if (> n 0) (message "write %d/%d" i n))) "next write" :bind nil)
     )
   )

(setq magit-repository-directories '("~/Dev"))

(after! ivy
  (setq ivy-initial-inputs-alist nil)
  (push '(+ivy/switch-workspace-buffer) ivy-display-functions-alist)
  )

(after! quickrun
  (quickrun-add-command "c++/c1z"
    '((:command . "clang++")
      (:exec    . ("%c -std=c++1z %o -o %e %s"
                   "%e %a"))
      (:remove  . ("%e")))
    :default "c++"))

(defun +advice/xref-set-jump (&rest args)
  (lsp-ui-peek--with-evil-jumps (evil-set-jump)))
(advice-add '+lookup/definition :before #'+advice/xref-set-jump)
(advice-add '+lookup/references :before #'+advice/xref-set-jump)


(defvar +my/xref-blacklist nil
  "List of paths that should not enable xref-find-* or dumb-jump-go")

;;; Override
;; This function is transitively called by xref-find-{definitions,references,apropos}
(after! xref
  ;; This is required to make `xref-find-references' not give a prompt.
  ;; `xref-find-references' asks the identifier (which has no text property)
  ;; and then passes it to `lsp-mode', which requires the text property at
  ;; point to locate the references.
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=29619
  (setq xref-prompt-for-identifier '(not xref-find-definitions
                                         xref-find-definitions-other-window
                                         xref-find-definitions-other-frame
                                         xref-find-references))

  (defun xref--show-xrefs (xrefs display-action &optional always-show-list)
    ;; PATCH
    (lsp-ui-peek--with-evil-jumps (evil-set-jump))

    ;; PATCH Jump to the first candidate
    (if (not (cdr xrefs))
        (xref--pop-to-location (car xrefs) display-action)
      (funcall xref-show-xrefs-function xrefs
               `((window . ,(selected-window))))
      ))
  )

(after! ivy-xref
  ;; (defun ivy-xref-show-xrefs (xrefs alist)
  ;;   (minibuffer-with-setup-hook #'hydra-ivy/body
  ;;      (minibuffer-with-setup-hook #'ivy-toggle-calling
  ;;        (ivy-read "xref: " (ivy-xref-make-collection xrefs)
  ;;                  :require-match t
  ;;                  :action #'(lambda (candidate)
  ;;                              (xref--show-location (cdr candidate) 'quit))))))
  ;; (push '(xref-find-references) ivy-display-functions-alist)
  (push '(ivy-xref-show-xrefs . nil) ivy-sort-functions-alist)
  )

(def-package! rust-mode
  :mode "\\.rs$"
  :config
  (set! :docset 'rust-mode "Rust")
  (map! :map rust-mode-map
        :leader
        :n "=" #'rust-format-buffer
        )
  )

(def-package! symbol-overlay
  :commands (symbol-overlay-put))

(def-package! lsp-rust
  :init (add-hook 'rust-mode-hook #'lsp-rust-enable)
  :config
  (set! :company-backend 'rust-mode '(company-lsp))
  )

(setq projectile-require-project-root t)

(after! counsel-projectile
  (ivy-add-actions
   'counsel-projectile-switch-project
   `(("b" counsel-projectile-switch-project-action-switch-to-buffer
      "jump to a project buffer")
     ("s" counsel-projectile-switch-project-action-save-all-buffers
      "save all project buffers")
     ("k" counsel-projectile-switch-project-action-kill-buffers
      "kill all project buffers")
     ("c" counsel-projectile-switch-project-action-compile
      "run project compilation command")
     ("e" counsel-projectile-switch-project-action-edit-dir-locals
      "edit project dir-locals")
     ("v" counsel-projectile-switch-project-action-vc
      "open project in vc-dir / magit / monky")
     ("xe" counsel-projectile-switch-project-action-run-eshell
      "invoke eshell from project root")
     ("xt" counsel-projectile-switch-project-action-run-term
      "invoke term from project root")
     ("_" counsel-projectile-switch-project-action-org-capture
      "org-capture into project"))))

(def-package! tldr
  :commands (tldr)
  :config
  (setq tldr-directory-path (concat doom-etc-dir "tldr/"))
  (set! :popup "^\\*tldr\\*" '((side . right)) '((select . t) (quit . t)))
  )

(def-package! treemacs
  :commands (treemacs treemacs-toggle)
  :config
  (setq treemacs-follow-after-init t)
  (treemacs-follow-mode +1)
  )

(def-package! treemacs-evil)

(def-package! treemacs-projectile
  :commands (treemacs-projectile treemacs-projectile-toggle))

(after! nav-flash
  ;; (defun nav-flash-show (&optional pos end-pos face delay)
  ;; ...
  ;; (let ((inhibit-point-motion-hooks t))
  ;; (goto-char pos)
  ;; (beginning-of-visual-line) ; work around args-out-of-range error when the target file is not opened
  (defun +advice/nav-flash-show (orig-fn &rest args)
    (ignore-errors (apply orig-fn args)))
  (advice-add 'nav-flash-show :around #'+advice/nav-flash-show))

(set! :popup "^\\*helpful" '((size . 0.4)))
(set! :popup "^\\*info.*" '((size . 80) (side . right)))
(set! :popup "^\\*Man.*" '((size . 80) (side . right)))

(let ((profile "~/.config/doom/profile.el"))
  (when (file-exists-p profile)
    (load-file profile)))
