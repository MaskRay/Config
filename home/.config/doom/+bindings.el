;;; private/my/+bindings.el -*- lexical-binding: t; -*-

(define-inline +my/prefix-M-x (prefix)
  (inline-quote
   (lambda () (interactive)
     (setq unread-command-events (string-to-list ,prefix))
     (call-interactively #'execute-extended-command))))

(map!
 ;; localleader
 :m ","    nil

 ;; Override default :n [b [w
 :nm "[" nil
 :nm "]" nil
 (:map prog-mode-map
   :m "["  #'lispyville-previous-opening
   :m "]"  #'lispyville-next-closing
   ;; Override default :n < >
   :nm "<" #'lispyville-next-opening
   :nm ">" #'lispyville-previous-closing

   :n "H"  #'lsp-ui-peek-jump-backward
   :n "L"  #'lsp-ui-peek-jump-forward
   )

 :m "M-h"  #'smart-up
 :m "M-l"  #'smart-down
 :n "M-."  #'+lookup/definition
 :n "M-j"  #'+my/find-definitions

 :n "C-s"  #'swiper
 :n "C-,"  #'+my/find-references
 :n ";"    (λ! (avy-goto-char-timer) (+my/find-definitions))
 :n "s"    #'avy-goto-char-timer

 :n "ga"   #'ff-find-other-file
 :n "gf"   #'+my/ffap
 :n "gs"   #'lsp-ui-find-workspace-symbol
 :n "go"   (λ! (message "%S" (text-properties-at (point))))

 :n "M-<"  #'previous-error
 :n "M->"  #'next-error

 :n "C-c P s" #'profiler-start
 :n "C-c P r" #'profiler-report
 :n "C-c P S" #'profiler-stop

 (:leader
   :n "SPC" #'+ivy/switch-workspace-buffer
   (:desc "app" :prefix "a"
     :n "g" (λ! (shell-command-on-region (point-min) (point-max) "genhdr" t t))
     :n "G" (λ! (shell-command-on-region (point-min) (point-max) "genhdr windows" t t))
     )
   (:desc "error" :prefix "e"
     :n "n" #'flycheck-next-error
     :n "p" #'flycheck-previous-error
     )
   (:prefix "f"
     :n "f" #'find-file
     :n "p" #'treemacs-projectile-toggle
     :n "P" #'treemacs-projectile
     :n "C-p" #'+private/find-in-config
     :n "C-S-p" #'+private/browse-config
     :n "t" #'treemacs-toggle
     :n "T" #'treemacs
     )
   (:prefix "g"
     :n "g" #'magit-status
     :n "l" #'+gist:list
     :n "*" (+my/prefix-M-x "magit-")
     )
   (:prefix "h"
     :n "C" #'helpful-command
     )
   (:prefix "o"
     :n "ee" #'+eshell/open
     :n "el" #'+eshell/open-last
     :n "ej" #'+eshell/next
     :n "ek" #'+eshell/previous
     :n "ep" #'+eshell/open-popup
     :n "es" #'+eshell/switch
     )
   (:prefix "p"
     :n "e" #'projectile-run-eshell
     :n "f" #'counsel-projectile-find-file
     :n "*" (+my/prefix-M-x "projectile-")
     )
   (:prefix "r"
     :n "l" #'ivy-resume
     )

   ;; Rebind to "S"
   (:desc "snippets" :prefix "S"
     :desc "New snippet"            :n  "n" #'yas-new-snippet
     :desc "Insert snippet"         :nv "i" #'yas-insert-snippet
     :desc "Find snippet for mode"  :n  "s" #'yas-visit-snippet-file
     :desc "Find snippet"           :n  "S" #'+default/find-in-snippets)

   (:desc "search" :prefix "s"
     :n "b" #'swiper-all
     :desc "Directory"              :nv "d" (λ! (+ivy/project-search t))
     :desc "Project"                :nv "s" #'+ivy/project-search
     :desc "Symbols"                :nv "i" #'imenu
     :desc "Symbols across buffers" :nv "I" #'imenu-anywhere
     :desc "Online providers"       :nv "o" #'+lookup/online-select
     )

   (:desc "toggle" :prefix "t"
     :n "d" #'doom/toggle-debug-mode
     :n "D" #'+my/realtime-elisp-doc
     )
   )

 :n "x" nil
 (:desc "xref" :prefix "x"
   :n "SPC" #'cquery/random
   :n ";" (λ! (avy-goto-char-timer) (+my/find-references))
   :n "b" #'cquery/base
   :n "d" #'cquery/derived
   :n "e" #'cquery/callers
   ;; caller hierarchy
   :n "c" #'cquery-call-hierarchy
   ;; callee hierarchy
   :n "C" (λ! (cquery-call-hierarchy t))
   ;; derived hierarchy
   :n "i" (λ! (cquery-inheritance-hierarchy t))
   ;; base hierarchy
   :n "I" #'cquery-inheritance-hierarchy
   :n "l" #'cquery-code-lens-mode
   :n "m" #'cquery-member-hierarchy
   :n "t" #'text-document/type-definition
   :n "v" #'cquery/vars
   :n "x" #'evil-delete-char
   )

 ;; ivy
 (:after ivy
   :map ivy-minibuffer-map
   "<tab>" #'ivy-call-and-recenter
   )

 (:after company
   (:map company-active-map
     ;; Don't interfere with `evil-delete-backward-word' in insert mode
     "C-v"        #'company-next-page
     "M-v"        #'company-previous-page
     "C-i"        #'company-complete-selection
     "SPC"        nil))
 )
