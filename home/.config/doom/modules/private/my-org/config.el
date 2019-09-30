;;; private/my-org/config.el -*- lexical-binding: t; -*-

(setq org-capture-templates
      '(("t" "Tasks" entry (file "refile.org")
         "* TODO %?\n  SCHEDULED: %^t")
        ("d" "Diary" entry (file+datetree "diary.org")
         "* %?")
        ("n" "Notes" entry (file "notes.org")
         "* %? :NOTE:")

        ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
        ;; {todo,notes,changelog}.org file is found in a parent directory.
        ;; Uses the basename from `+org-capture-todo-file',
        ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
        ("p" "Project notes" entry     ; {project-root}/notes.org
         (file+headline +org-capture-project-notes-file "Inbox")
         "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)))
