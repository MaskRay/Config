;;; private/my/+bindings.el -*- lexical-binding: t; -*-

(define-inline +my/prefix-M-x (prefix)
  (inline-quote
   (lambda () (interactive)
     (setq unread-command-events (string-to-list ,prefix))
     (call-interactively #'execute-extended-command))))

(define-inline +my/simulate-key (key)
  (inline-quote
   (lambda () (interactive)
     (setq prefix-arg current-prefix-arg)
     (setq unread-command-events (listify-key-sequence (read-kbd-macro ,key))))))

(general-define-key :states '(normal visual motion) "x" nil)

(map!
 ;; localleader
 :m ","    nil

 (:map prog-mode-map
   ;; Override default :n < > ( )
   ;; :nm "<" #'lispyville-previous-opening
   ;; :nm ">" #'lispyville-next-closing
   :m "RET" #'+ivy/switch-workspace-buffer

   :n "C-h" #'lispyville-backward-up-list
   :n "C-j" #'lispyville-forward-sexp
   :n "C-k" #'lispyville-backward-sexp
   :n "C-l" #'lispyville-up-list

   :n "H"  #'lsp-ui-peek-jump-backward
   :n "L"  #'lsp-ui-peek-jump-forward
   :m "C-H"  #'+my/xref-jump-backward-file
   :m "C-L"  #'+my/xref-jump-forward-file
   )

 ;; :n "M-u" (+my/simulate-key "[")
 ;; :n "M-i" (+my/simulate-key "]")
 :n "M-;"  #'eval-expression
 :n "M-."  #'+lookup/definition
 :n "M-f"  #'swiper
 :n "M-j"  #'+my/find-definitions

 :n "C-1" #'+popup/raise
 :n "C-c a" #'org-agenda
 :n "C-,"  #'+my/find-references
 :n "M-,"  (λ! (+my/find-references (+my//folder-param t)))
 ;; all symbols
 :n ";"    (λ! (if lsp-mode
                    (progn (+my/avy-document-symbol t)
                           (+my/find-definitions))
                  (avy-goto-word-0 nil)))
 ;; outline
 :n "z;"   (λ! (+my/avy-document-symbol nil))

 :n "ga"   #'+my/workspace-symbol
 :n "gA"   (λ! (setq current-prefix-arg t) (call-interactively #'+my/workspace-symbol))
 :n "gc"   #'evilnc-comment-or-uncomment-lines
 :n "gf"   #'+my/ffap
 :n "go"   (λ! (message "%S" (text-properties-at (point))))

 :n "[ M-u" #'symbol-overlay-switch-backward
 :n "] M-i" #'symbol-overlay-switch-forward

 (:prefix "x"
   :n ";" (λ! (+my/avy-document-symbol t) (+my/find-references))

   ;; $ccls/inheritance
   :n "b" (λ! (ccls/base 1))
   :n "B" (λ! (ccls/base 3))
   :n "d" (λ! (ccls/derived 1))
   :n "D" (λ! (ccls/derived 3))
   :n "i" #'ccls-inheritance-hierarchy         ; base hierarchy
   :n "I" (λ! (ccls-inheritance-hierarchy t)) ; derived hierarchy

   ;; $ccls/call
   :n "c" #'ccls/caller
   :n "C" #'ccls/callee
   ;; caller hierarchy
   :n "e" #'ccls-call-hierarchy
   ;; callee hierarchy
   :n "E" (λ! (ccls-call-hierarchy t))

   ;; $ccls/member
   :n "s" (λ! (ccls/member 2))   ; 2 (Type) => nested classes/namespace members
   :n "f" (λ! (ccls/member 3))   ; 3 (Func) => member functions
   :n "m" (λ! (ccls/member 0))   ; other => member variables
   :n "M" #'ccls-member-hierarchy

   :n "L" #'ccls-code-lens-mode
   :n "t" #'lsp-goto-type-definition
   ;; https://github.com/maskray/ccls/blob/master/src/messages/ccls_vars.cc#L15
   :n "v" (λ! (ccls/vars 3))           ; field or local variable
   :n "V" (λ! (ccls/vars 1))           ; field
   :n "C-v" (λ! (ccls/vars 7))         ; any
   :n "x" #'evil-delete-char)

 (:prefix "C-x"
   :n "e"  #'pp-eval-last-sexp
   :n "u" #'link-hint-open-link
   )
 )

(map! :leader
   "SPC" #'+ivy/switch-workspace-buffer
   ;; :n "M-u" (+my/simulate-key "SPC [")
   ;; :n "M-i" (+my/simulate-key "SPC ]")
   (:prefix ("a" . "app")
     :desc "genhdr" :n "g"
     (λ! (shell-command-on-region (point-min) (point-max) "genhdr" t t))
     :desc "genhdr windows" :n "G"
     (λ! (shell-command-on-region (point-min) (point-max) "genhdr windows" t t))
     )
   (:prefix "b"
     :desc "Last buffer" :n "b" #'evil-switch-to-windows-last-buffer
     "l" #'ivy-switch-buffer
     )
   (:prefix ("e" . "error")
     "n" #'flycheck-next-error
     "p" #'flycheck-previous-error
     )
   (:prefix "g"
     "*" (+my/prefix-M-x "magit-")
     :desc "Magit blame"               "b"   #'magit-blame-addition
     :desc "Magit switch branch"       "B"   #'magit-branch-checkout
     "q" #'git-link
     )
   (:prefix "h"
     "C" #'helpful-command
     )
   (:prefix ("l" . "lsp")
     "=" #'lsp-format-buffer
     "a" #'lsp-execute-code-action
     "l" #'lsp-ui-sideline-mode
     "d" #'lsp-ui-doc-mode
     "e" #'flymake-show-diagnostics-buffer
     "i" #'lsp-ui-imenu
     "r" #'lsp-rename
     "R" #'lsp-restart-workspace
     "w" #'lsp-ui-peek-find-workspace-symbol
     )
   :desc "lispyville" :n "L" (+my/prefix-M-x "lispyville ")
   (:prefix "o"
     "d" #'+debugger:start
     "o" #'symbol-overlay-put
     "q" #'symbol-overlay-remove-all
     )
   (:prefix "p"
     "e" #'projectile-run-eshell
     "f" #'counsel-projectile-find-file
     "*" (+my/prefix-M-x "projectile-")
     )
   (:prefix "r"
     "l" #'ivy-resume
     )

   ;; Rebind to "S"
   (:prefix ("S" . "snippets")
     :desc "New snippet"            "n" #'yas-new-snippet
     :desc "Insert snippet"         "i" #'yas-insert-snippet
     :desc "Find snippet for mode"  "s" #'yas-visit-snippet-file
     :desc "Find snippet"           "S" #'+default/find-in-snippets)

   (:prefix ("s" . "search")
     "b" #'swiper-all
     :desc "Directory"              "d" #'+ivy/project-search-from-cwd
     :desc "Project"                "/" #'+ivy/project-search
     "s" (λ! (minibuffer-with-setup-hook
                  (lambda () (insert ivy--default)) (+ivy/project-search)))
     :desc "Symbols"                "i" #'imenu
     :desc "Symbols across buffers" "I" #'imenu-anywhere
     :desc "Online providers"       "o" #'+lookup/online-select
     )

   (:prefix ("t" . "toggle")
     "d" #'toggle-debug-on-error
     "D" #'+my/realtime-elisp-doc
     )
   )

(map!
 (:after evil-collection-info
   :map Info-mode-map
   "/" #'Info-search
   "?" #'Info-search-backward
   )

 ;; ivy
 (:after ivy
   :map ivy-minibuffer-map
   "<tab>" #'ivy-call-and-recenter
   "C-;"   #'ivy-posframe-avy
   "C-b"   #'backward-char
   "C-f"   #'forward-char
   )

 (:after company
   (:map company-active-map
     ;; Don't interfere with `evil-delete-backward-word' in insert mode
     "C-v"        #'company-next-page
     "M-v"        #'company-previous-page
     "C-i"        #'company-complete-selection
     [tab]        #'company-complete-selection
     "RET"        nil
     [return]     nil
     "SPC"        nil))

 (:after realgud
   (:map realgud-track-mode-map
     :in ";" #'realgud-window-src-undisturb-cmd
     :in "C-j" (λ! (realgud:cmd-repeat-last) (realgud-window-src-undisturb-cmd)))
   (:map realgud:shortkey-mode-map
     ;; :n "e" (λ! (realgud:cmd-run-command (thing-at-point 'symbol) "eval"))
     :n "i" #'realgud-window-cmd-undisturb-src
     :n "t" #'realgud:cmd-tbreak
     :n "U" #'realgud:cmd-until
     :n "1" (λ! (+my/realgud-eval-nth-name-forward 1))
     :n "2" (λ! (+my/realgud-eval-nth-name-forward 2))
     :n "3" (λ! (+my/realgud-eval-nth-name-forward 3))
     :n "4" (λ! (+my/realgud-eval-nth-name-forward 4))
     :n "5" (λ! (+my/realgud-eval-nth-name-forward 5))
     :n "6" (λ! (+my/realgud-eval-nth-name-forward 6))
     :n "7" (λ! (+my/realgud-eval-nth-name-forward 7))
     :n "8" (λ! (+my/realgud-eval-nth-name-forward 8))
     :n "9" (λ! (+my/realgud-eval-nth-name-forward 9))
     :n "M-1" (λ! (+my/realgud-eval-nth-name-backward 1))
     :n "M-2" (λ! (+my/realgud-eval-nth-name-backward 2))
     :n "M-3" (λ! (+my/realgud-eval-nth-name-backward 3))
     :n "M-4" (λ! (+my/realgud-eval-nth-name-backward 4))
     :n "M-5" (λ! (+my/realgud-eval-nth-name-backward 5))
     :n "M-6" (λ! (+my/realgud-eval-nth-name-backward 6))
     :n "M-7" (λ! (+my/realgud-eval-nth-name-backward 7))
     :n "M-8" (λ! (+my/realgud-eval-nth-name-backward 8))
     :n "M-9" (λ! (+my/realgud-eval-nth-name-backward 9))
     )
   )
 )
