;;;  -*- lexical-binding: t; -*-

(setq org-capture-templates
  '(
    ("d" "Diary" entry
     (file+datetree "diary.org")
     "* %?" :kill-buffer t)
    ("n" "Notes" entry
     (file+headline +org-default-notes-file "Inbox")
     "* %u %?\n%i" :prepend t :kill-buffer t)
    ("t" "Todo" entry
     (file+headline +org-default-todo-file "Inbox")
     "* [ ] %?\n%i" :prepend t :kill-buffer t)

    ))
