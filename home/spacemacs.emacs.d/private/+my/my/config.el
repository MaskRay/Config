;; Override spacemacs-base/init-bookmark: do not automatically save bookmark list each time it is modified
(setq bookmark-save-flag t)

(setq org-directory "~/org"
      org-default-notes-file "~/org/refile.org"
      org-agenda-files '("~/org/refile.org" "~/org/gtd.org" "~/org/notes.org")
      org-startup-indented t                           ; org-indent-mode
      org-treat-S-cursor-todo-selection-as-state-change nil
      org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
        (sequence "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))
      org-todo-keyword-faces
      '(("TODO" :foreground "#cc9393" :weight bold)
        ("NEXT" :foreground "#8cd0d3" :weight bold)
        ("DONE" :foreground "#afd8af" :weight bold)
        ("HOLD" :foreground "#dc8cc3" :weight bold)
        ("CANCELLED" :foreground "#bfebbf" :weight bold))
      org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
        ("HOLD" ("HOLD" . t))
        (done ("HOLD"))
        ("TODO" ("CANCELLED") ("HOLD"))
        ("NEXT" ("CANCELLED") ("HOLD"))
        ("DONE" ("CANCELLED") ("HOLD")))
      org-capture-templates
      '(("t" "Tasks" entry (file "~/org/refile.org")
         "* TODO %?\n  SCHEDULED: %^t")
        ("d" "Diary" entry (file+datetree "~/org/diary.org")
         "* %?")
        ("n" "Notes" entry (file "~/org/notes.org")
         "* %? :NOTE:")
        ("m" "Meeting" entry (file "~/org/refile.org")
         "* MEETING with %? :MEETING:\n%U")
        ("p" "Phone call" entry (file "~/org/refile.org")
         "* PHONE %? :PHONE:\n%U")
        ))

(setq browse-url-browser-function 'browse-url-generic
      engine/browser-function 'browse-url-generic
      browse-url-generic-program (or (getenv "BROWSER") "chrome"))

(setq open-junk-file-format "/tmp/.junk/%Y/%m/%d-%H%M%S.")
