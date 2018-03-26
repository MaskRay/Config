;;; private/my/+bindings.el -*- lexical-binding: t; -*-

(map!
 ;; localleader
 :m ","    nil

 :n "C-s"  #'swiper
 :n "M-."  #'+lookup/definition
 :n "M-j"  #'+my/find-definitions
 :n "C-,"  #'+my/find-references
 :n ";"    (lambda () (interactive) (avy-goto-char-timer) (+my/find-definitions))
 :n "s"    #'avy-goto-char-timer

 :n "gf"   #'+my/ffap
 :n "gs"   #'lsp-ui-find-workspace-symbol
 :n "go"   (lambda () (interactive) (message "%S" (text-properties-at (point))))
 :n "H"    #'lsp-ui-peek-jump-backward
 :n "L"    #'lsp-ui-peek-jump-forward

 :n "M-<"  #'previous-error
 :n "M->"  #'next-error

 :n "C-c P s" #'profiler-start
 :n "C-c P r" #'profiler-report
 :n "C-c P S" #'profiler-stop

 (:leader
   :n "SPC" #'+ivy/switch-workspace-buffer
   (:desc "error" :prefix "e"
     :n "n" #'flycheck-next-error
     :n "p" #'flycheck-previous-error
     )
   (:prefix "f"
     :n "f" #'find-file
     )
   (:prefix "g"
     :n "g" #'magit-status
     :n "l" #'+gist:list
     )
   (:desc "jump" :prefix "j"
     :n "i" #'imenu
     )
   (:prefix "p"
     :n "e" #'projectile-run-eshell
     :n "f" #'counsel-projectile-find-file
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
     :desc "Project"                :nv "p" #'+ivy/project-search
     )

   (:desc "toggle" :prefix "t"
     :n "d" #'doom/toggle-debug-mode
     :n "D" #'+my/realtime-elisp-doc
     )
   )

 ;; :n "x" nil
 (:desc "xref" :prefix "x"
   :n "SPC" #'cquery/random
   :n ";" (lambda () (interactive) (avy-goto-char-timer) (+my/find-references))
   :n "b" #'cquery/base
   :n "d" #'cquery/derived
   :n "e" #'cquery/callers
   :n ; callers
   :n "c" #'cquery-call-hierarchy
   :n ; callees
   :n "C" (lambda () (interactive) (cquery-call-hierarchy t))
   :n ; derived
   :n "i" (lambda () (interactive) (cquery-inheritance-hierarchy t))
   :n ; base
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
     "C-w"        nil
     "C-v"        #'company-next-page
     "M-v"        #'company-previous-page
     "C-o"        #'company-search-kill-others
     "C-j"        #'company-select-next
     "C-k"        #'company-select-previous
     "C-h"        #'company-quickhelp-manual-begin
     "C-S-h"      #'company-show-doc-buffer
     "C-S-s"      #'company-search-candidates
     "C-s"        #'company-filter-candidates
     "C-i"        #'company-complete-selection
     "RET"        nil
     "SPC"        nil
     [return]     nil
     [tab]        nil
     [backtab]    nil)
   ;; Automatically applies to `company-filter-map'
   (:map company-search-map
     "C-j"        #'company-search-repeat-forward
     "C-k"        #'company-search-repeat-backward
     "C-s"        (lambda! (company-search-abort) (company-filter-candidates))
     [escape]     #'company-search-abort))
 )
