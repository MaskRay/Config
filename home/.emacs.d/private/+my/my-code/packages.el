(defconst my-code-packages
  '(
    cc-mode
    dumb-jump
    emacs-lisp
    evil
    evil-snipe
    flycheck
    git-link
    haskell-mode
    ivy-posframe
    lispy
    lispyville
    lsp-haskell
    lsp-rust
    ;; (lsp-mode :location local)
    lsp-mode
    ;; (lsp-ui :location local)
    lsp-ui
    modern-cpp-font-lock
    realgud
    smartparens
    ))

(defun my-code/post-init-cc-mode ()
  (delq 'spacemacs//c-toggle-auto-newline c-mode-common-hook)
  (dolist (mode c-c++-modes)
    (spacemacs/declare-prefix-for-mode mode "mx" "format")
    (spacemacs/set-leader-keys-for-major-mode mode
      "xf" 'clang-format-region
      "db" (lambda ()
             (interactive)
             (evil-open-above 1)
             (insert "volatile static int z=0;while(!z)asm(\"pause\");")
             (evil-normal-state)
             )))

  ;; https://github.com/radare/radare2
  (c-add-style
   "radare2"
   '((c-basic-offset . 2)
     (indent-tabs-mode . t)
     (c-auto-align-backslashes . nil)
     (c-offsets-alist
      (arglist-intro . ++)
      (arglist-cont . ++)
      (arglist-cont-nonempty . ++)
      (statement-cont . ++)
      )))

  (c-add-style
   "my-cc" '("user"
             (c-basic-offset . 2)
             (c-offsets-alist
              . ((innamespace . 0)
                 (access-label . -)
                 (case-label . 0)
                 (member-init-intro . +)
                 (topmost-intro . 0)
                 (arglist-cont-nonempty . +)))))
  (setf (alist-get 'other c-default-style) "my-cc")
  )

(defun my-code/post-init-dumb-jump ()
  ;; Don't use dumb-jump-go in large code base.
  (advice-add 'dumb-jump-go :around #'my-advice/dumb-jump-go))

(defun my-code/post-init-git-link ()
  (with-eval-after-load 'git-link
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
    ))

(defun my-code/post-init-flycheck ()
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

(defun my-code/post-init-haskell-mode ()
  (with-eval-after-load 'haskell-mode
    (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
    ;; (add-hook 'haskell-mode-hook 'structured-haskell-mode)
    ;; (intero-global-mode 1)
    ;; (add-hook 'haskell-mode-hook 'intero-mode)
    ;; (add-to-list 'spacemacs-jump-handlers-haskell-mode 'intero-goto-definition)
    )
  )

(defun my-code/init-lispy ()
  (use-package lispy
    :init
    (add-hook 'emacs-lisp-mode-hook #'lispy-mode)
    ))

(defun my-code/init-lispyville ()
  (use-package lispyville
    :init
    (add-hook 'lispy-mode-hook #'lispyville-mode)
    :config
    (lispyville-set-key-theme
     '(operators
       c-w
       slurp/barf-lispy
       (escape insert)
       (additional-movement normal visual motion)))
    ))

(defun my-code/init-ivy-posframe ()
  (use-package ivy-posframe
    :config
    (setq ivy-display-function #'ivy-posframe-display)
    (ivy-posframe-enable)
    ))

(defun my-code/post-init-emacs-lisp ()
  (evil-define-key 'insert emacs-lisp-mode-map
    "[" (lambda () (interactive) (insert ?\())
    "]" (lambda () (interactive) (insert ?\)))
    "(" (lambda () (interactive) (insert ?\[))
    ")" (lambda () (interactive) (insert ?\]))
    )

  (evil-define-key 'normal emacs-lisp-mode-map "C-c /" #'lispy-splice)

  (require 'projectile)
  (add-to-list 'projectile-project-root-files-functions #'my-projectile/dotemacs-elpa-package)
  )

(defun my-code/post-init-evil ()
  (add-to-list 'evil-emacs-state-modes 'xref--xref-buffer-mode)

  (with-eval-after-load 'evil-mc
    (dolist (key '("C-n" "C-p" "C-t"))
      (evil-define-key 'normal evil-mc-key-map (kbd key) nil)
      (evil-define-key 'visual evil-mc-key-map (kbd key) nil)
      )
    (evil-define-key 'normal evil-mc-key-map
      (kbd "M-n") #'evil-mc-make-and-goto-next-match
      (kbd "M-p") #'evil-mc-make-and-goto-prev-match
      (kbd "M-t") #'evil-mc-skip-and-goto-next-match
      ))

  (my/define-key
   evil-motion-state-map
   "M-g b" #'my-avy/goto-paren
   "M-g c" #'my-avy/goto-conditional
   )

  (spacemacs/set-leader-keys "cb" #'my/compilation-buffer)

  (my/define-key
   evil-normal-state-map
   "SPC SPC" #'counsel-projectile-switch-to-buffer
   "gd" #'my-xref/find-definitions
   "gf" #'my/ffap
   "gs" #'lsp-ui-find-workspace-symbol
   "go" (lambda () (interactive) (message "%S" (text-properties-at (point))))
   "C-j" #'my-xref/find-definitions
   "C-," #'my-xref/find-references
   ";" (lambda () (interactive) (avy-goto-char-timer) (my-xref/find-definitions))
   "H" #'lsp-ui-peek-jump-backward
   "L" #'lsp-ui-peek-jump-forward
   "x" nil
   "x SPC" #'cquery/random
   "x;" (lambda () (interactive) (avy-goto-char-timer) (my-xref/find-references))
   "xb" #'cquery/base
   "xd" #'cquery/derived
   "xe" #'cquery/callers
   ;; callers
   "xc" #'cquery-call-hierarchy
   ;; callees
   "xC" (lambda () (interactive) (cquery-call-hierarchy t))
   ;; derived
   "xi" (lambda () (interactive) (cquery-inheritance-hierarchy t))
   ;; base
   "xI" #'cquery-inheritance-hierarchy
   "xl" #'cquery-code-lens-mode
   "xm" #'cquery-member-hierarchy
   "xt" #'text-document/type-definition
   "xv" #'cquery/vars
   "xx" #'evil-delete-char
   "M-<" #'previous-error
   "M->" #'next-error

   "<Backspace>" #'spacemacs/evil-search-clear-highlight
   "C-c P s" #'profiler-start
   "C-c P r" #'profiler-report
   "C-c P S" #'profiler-stop
   )
  (my/define-key
   evil-motion-state-map
   "L" nil
   )
  (my/define-key
   evil-insert-state-map
   "C-x C-l" #'my/expand-line
   )

  (defhydra hydra/ref (evil-normal-state-map "x")
    "reference"
    ("j" lsp-ui-find-next-reference)
    ("k" lsp-ui-find-prev-reference)
    )

  (evil-define-motion evil-end-of-line (count)
    "Move the cursor to the end of the current line.
If COUNT is given, move COUNT - 1 lines downward first."
    :type inclusive
    (move-end-of-line count)
    (when evil-track-eol
      (setq temporary-goal-column most-positive-fixnum
            this-command 'next-line))
    (unless (and (evil-visual-state-p) evil-v$-gets-eol)
      (evil-adjust-cursor)
      (when (eolp)
        ;; prevent "c$" and "d$" from deleting blank lines
        (setq evil-this-type 'exclusive))))
  (setq evil-v$-gets-eol nil)

  (spacemacs/set-leader-keys
    "aa" (lambda ()
           (interactive)
           (let ((f (file-name-base (buffer-file-name))))
             (set-buffer "*ansi-term-1*")
             (term-send-raw-string (format "\C-umake %s && ./%s \C-m" f f))))
    "ag" (lambda () (interactive) (shell-command-on-region (point-min) (point-max) "genhdr" t t))
    "aG" (lambda () (interactive) (shell-command-on-region (point-min) (point-max) "genhdr windows" t t))
    "TD" #'my/realtime-elisp-doc
    ;; previous/next modified hunk in git
    "gp" (lambda () (interactive) (git-gutter+-next-hunk -1))
    "gn" (lambda () (interactive) (git-gutter+-next-hunk 1))
    )

  (add-hook 'TeX-mode-hook #'spacemacs/toggle-auto-fill-mode-off)

  (with-eval-after-load 'smartparens
    (assq-delete-all :unmatched-expression sp-message-alist))
  )

(defun my-code/post-init-evil-snipe ()
  ;; Rebind surround to S instead of s, so we can use s for avy
  (evil-define-key 'operator evil-surround-mode-map "S" 'evil-surround-edit)
  (evil-define-key 'visual evil-surround-mode-map "S" 'evil-surround-region)
  (evil-snipe-mode -1)

  ;; avy
  (evil-define-key '(normal motion) global-map "s" #'avy-goto-char-timer)
  (evil-define-key '(visual operator) evil-surround-mode-map "s" #'avy-goto-char-timer)
  (setq avy-timeout-seconds 0.2)
  )

(defun my-code/post-init-lsp-mode ()
  (use-package lsp-mode
    :config
    (add-to-list 'spacemacs-jump-handlers-d-mode 'company-dcd-goto-definition)
    (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc)) ;; in flycheck.el

    (setq company-quickhelp-delay 0
          company-show-numbers t
          company-idle-delay 0.1)

    (require 'lsp-imenu)
    (add-hook 'lsp-after-open-hook #'lsp-enable-imenu)

    (advice-add 'spacemacs/jump-to-definition :before #'my-advice/xref-set-jump)
    (advice-add 'spacemacs/jump-to-reference :before #'my-advice/xref-set-jump)

    ;;; Override
    (dolist (mode '("c" "c++" "go" "haskell" "javascript" "python" "rust"))
      (let ((handler (intern (format "spacemacs-jump-handlers-%s-mode" mode))))
        (add-to-list handler 'lsp-ui-peek-find-definitions))
      (let ((handler (intern (format "spacemacs-reference-handlers-%s-mode" mode))))
        (add-to-list handler 'lsp-ui-peek-find-references)))

    (spacemacs/set-leader-keys-for-minor-mode 'lsp-mode
      "lA" #'lsp-ui-peek-find-workspace-symbol
      "lf" #'cquery-freshen-index
      "lF" #'lsp-format-buffer
      "lL" #'lsp-ui-sideline-mode
      "lD" #'lsp-ui-doc-mode
      "lp" #'cquery-preprocess-file
      "lr" #'lsp-rename
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
                             (/= (logand (ht-get x "role" 0) 8) 0)))]
             (if (> n 0) (message "read %d/%d" i n))) "prev read" :bind nil)
      ("r" (-let [(i . n) (lsp-ui-find-next-reference
                           (lambda (x)
                             (/= (logand (ht-get x "role" 0) 8) 0)))]
             (if (> n 0) (message "read %d/%d" i n))) "next read" :bind nil)
      ("W" (-let [(i . n) (
                           (lambda (x)
                             (/= (logand (ht-get x "role" 0) 16) 0)))]
             (if (> n 0) (message "write %d/%d" i n))) "prev write" :bind nil)
      ("w" (-let [(i . n) (lsp-ui-find-next-reference
                           (lambda (x)
                             (/= (logand (ht-get x "role" 0) 16) 0)))]
             (if (> n 0) (message "write %d/%d" i n))) "next write" :bind nil)
      )

    (dolist (mode c-c++-modes)
      (spacemacs/set-leader-keys-for-major-mode mode
        "a" #'cquery/references-address
        "r" #'cquery/references-read
        "w" #'cquery/references-write
        ;; callers
        "c" #'cquery-call-hierarchy
        ;; callees
        "C" (lambda () (interactive) (cquery-call-hierarchy t))
        ))

    (define-key evil-motion-state-map (kbd "M-<down>") 'lsp-ui-find-next-reference)
    (define-key evil-motion-state-map (kbd "M-<up>") 'lsp-ui-find-previous-reference)
    )
  )

(defun my-code/post-init-lsp-ui ()
  (use-package lsp-ui
    :demand t
    :config
    (setq lsp-ui-doc-include-signature nil)  ; don't include type signature in the child frame

    ;; TODO slow https://github.com/emacs-lsp/lsp-ui/issues/45
    ;; (setq lsp-ui-flycheck-enable nil)
    (setq lsp-ui-sideline-enable nil)
    (setq lsp-ui-sideline-show-symbol nil)  ; don't show symbol on the right of info
    (setq lsp-ui-sideline-ignore-duplicate t)

    (setq lsp-ui-peek-expand-function (lambda (xs) (mapcar #'car xs)))

    (my/define-key
     lsp-ui-peek-mode-map
     "h" #'lsp-ui-peek--select-prev-file
     "j" #'lsp-ui-peek--select-next
     "k" #'lsp-ui-peek--select-prev
     "l" #'lsp-ui-peek--select-next-file
     )
    ))

(defun my-code/init-lsp-haskell ()
  (use-package lsp-haskell
    :commands lsp-haskell-enable
    :init (add-hook 'haskell-mode-hook #'lsp-haskell-enable)
    )
  )

(defun my-code/init-lsp-rust ()
  (use-package lsp-rust
    :defer t
    :after lsp-mode
    :config
    (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
    (add-hook 'rust-mode-hook #'lsp-rust-enable)))

(defun my-code/init-modern-cpp-font-lock ()
  (use-package modern-cpp-font-lock
    :defer t
    :init
    (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
    :config
    (spacemacs|diminish modern-c++-font-lock-mode)
    ))

(defun my-code/post-init-realgud ()
  (with-eval-after-load 'realgud
    ;; It was bound to [mouse-2], but [mouse-1] is more convenient.
    (define-key realgud:shortkey-mode-map [mouse-1] #'realgud:tooltip-eval)
    (define-key realgud:shortkey-mode-map (kbd "p") #'my/realgud-eval-region-or-word-at-point)
    (define-key realgud:shortkey-mode-map (kbd "C-o") #'evil-execute-in-normal-state)
    (define-key realgud:shortkey-mode-map (kbd "w") #'evil-forward-word-begin)
    (define-key realgud:shortkey-mode-map (kbd "W") #'evil-forward-WORD-begin)

    ;; Don't use default bindings in c-c++/packages.el
    (evilified-state-evilify-map realgud:shortkey-mode-map
      :eval-after-load realgud
      :mode realgud-short-key-mode
      :bindings
      "C-j" 'lsp-ui-peek-find-definitions
      "C-," 'lsp-ui-peek-find-references
      "C-t" 'lsp-ui-peek-jump-backward
      "C-p" 'lsp-ui-peek-jump-forward

      "J" 'realgud:cmd-jump
      "n" 'realgud:cmd-next
      "s" 'realgud:cmd-step

      "bb" 'realgud:cmd-break
      "bc" 'realgud:cmd-clear
      "bd" 'realgud:cmd-delete
      "bs" 'realgud:cmd-disable
      "be" 'realgud:cmd-enable

      "f" 'realgud:cmd-finish
      "c" 'realgud:cmd-continue
      "e" 'realgud:cmd-eval-dwim
      "r" 'realgud:cmd-restart
      "q" 'realgud:cmd-quit
      "S" 'realgud-window-cmd-undisturb-src)
    ;; (evil-define-key 'evilified org-agenda-mode-map (kbd "v") nil)

    ;; Rebind 1 .. 9 to M-1 .. M-9
    ;; (define-key realgud:shortkey-mode-map (kbd "M-1") #'realgud-goto-arrow1)
    ;; (define-key realgud:shortkey-mode-map (kbd "M-2") #'realgud-goto-arrow2)
    ;; (define-key realgud:shortkey-mode-map (kbd "M-3") #'realgud-goto-arrow3)
    ;; (define-key realgud:shortkey-mode-map (kbd "M-4") #'realgud:goto-loc-hist-4)
    ;; (define-key realgud:shortkey-mode-map (kbd "M-5") #'realgud:goto-loc-hist-5)
    ;; (define-key realgud:shortkey-mode-map (kbd "M-6") #'realgud:goto-loc-hist-6)
    ;; (define-key realgud:shortkey-mode-map (kbd "M-7") #'realgud:goto-loc-hist-7)
    ;; (define-key realgud:shortkey-mode-map (kbd "M-8") #'realgud:goto-loc-hist-8)
    ;; (define-key realgud:shortkey-mode-map (kbd "M-9") #'realgud:goto-loc-hist-9)

    (define-key realgud:shortkey-mode-map (kbd "1") (lambda () (interactive) (my/realgud-eval-nth-name-forward 1)))
    (define-key realgud:shortkey-mode-map (kbd "2") (lambda () (interactive) (my/realgud-eval-nth-name-forward 2)))
    (define-key realgud:shortkey-mode-map (kbd "3") (lambda () (interactive) (my/realgud-eval-nth-name-forward 3)))
    (define-key realgud:shortkey-mode-map (kbd "4") (lambda () (interactive) (my/realgud-eval-nth-name-forward 4)))
    (define-key realgud:shortkey-mode-map (kbd "5") (lambda () (interactive) (my/realgud-eval-nth-name-forward 5)))
    (define-key realgud:shortkey-mode-map (kbd "6") (lambda () (interactive) (my/realgud-eval-nth-name-forward 6)))
    (define-key realgud:shortkey-mode-map (kbd "7") (lambda () (interactive) (my/realgud-eval-nth-name-forward 7)))
    (define-key realgud:shortkey-mode-map (kbd "8") (lambda () (interactive) (my/realgud-eval-nth-name-forward 8)))
    (define-key realgud:shortkey-mode-map (kbd "9") (lambda () (interactive) (my/realgud-eval-nth-name-forward 9)))

    (define-key realgud:shortkey-mode-map (kbd "M-1") (lambda () (interactive) (my/realgud-eval-nth-name-backward 1)))
    (define-key realgud:shortkey-mode-map (kbd "M-2") (lambda () (interactive) (my/realgud-eval-nth-name-backward 2)))
    (define-key realgud:shortkey-mode-map (kbd "M-3") (lambda () (interactive) (my/realgud-eval-nth-name-backward 3)))
    (define-key realgud:shortkey-mode-map (kbd "M-4") (lambda () (interactive) (my/realgud-eval-nth-name-backward 4)))
    (define-key realgud:shortkey-mode-map (kbd "M-5") (lambda () (interactive) (my/realgud-eval-nth-name-backward 5)))
    (define-key realgud:shortkey-mode-map (kbd "M-6") (lambda () (interactive) (my/realgud-eval-nth-name-backward 6)))
    (define-key realgud:shortkey-mode-map (kbd "M-7") (lambda () (interactive) (my/realgud-eval-nth-name-backward 7)))
    (define-key realgud:shortkey-mode-map (kbd "M-8") (lambda () (interactive) (my/realgud-eval-nth-name-backward 8)))
    (define-key realgud:shortkey-mode-map (kbd "M-9") (lambda () (interactive) (my/realgud-eval-nth-name-backward 9)))
    ))

(defun my-code/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    (define-key smartparens-mode-map (kbd "M-]") 'sp-unwrap-sexp)
    ;; For termite modify_other_keys or xterm modifyOtherKeys to work,
    ;; we cannot bind M-[
    ;; (define-key smartparens-mode-map (kbd "M-[") 'sp-backward-unwrap-sexp)
    (define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
    (define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)
    (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
    (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
    (define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
    (define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)
    (define-key smartparens-mode-map (kbd "C-c \"") (lambda () (interactive) (sp-wrap-with-pair "\"")))
    (define-key smartparens-mode-map (kbd "C-c '") (lambda () (interactive) (sp-wrap-with-pair "'")))
    (define-key smartparens-mode-map (kbd "C-c (") (lambda () (interactive) (sp-wrap-with-pair "(")))
    (define-key smartparens-mode-map (kbd "C-c [") (lambda () (interactive) (sp-wrap-with-pair "[")))
    (define-key smartparens-mode-map (kbd "C-c {") (lambda () (interactive) (sp-wrap-with-pair "{")))
    )
  )

;;; packages.el ends here
