(define (first-binding)
  (xbindkey-function '(control semicolon) C-semicolon)
  )

(define (reset-first-binding)
  (ungrab-all-keys)
  (remove-all-keys)
  (first-binding)
  (grab-all-keys))

(define (bind key command)
  (xbindkey-function key
                     (lambda() (run-command command)
                               (reset-first-binding)))
  )

(define (C-semicolon)
  (ungrab-all-keys)
  (remove-all-keys)
  (bind 'c "wmctrl -xa XTerm || xterm")
  (bind 'e "wmctrl -xa Emacs || emacsclient -c -n")
  (bind 'v "wmctrl -xa Vim || gvim")
  (bind 'f "wmctrl -xa Firefox || firefox")
  (bind 'i "wmctrl -xa Evince || evince")
  (xbindkey-function '(control g) reset-first-binding)
  (grab-all-keys))

(first-binding)
