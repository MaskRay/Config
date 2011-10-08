(define (first-binding)
  (xbindkey-function '(control semicolon) C-semicolon)
  )

(define (reset-first-binding)
  (ungrab-all-keys)
  (remove-all-keys)
  (first-binding)
  (grab-all-keys))

(define (C-semicolon)
  (ungrab-all-keys)
  (remove-all-keys)
  (xbindkey-function 'c
		     (lambda ()
		       (run-command "wmctrl -xa XTerm || xterm")
		       (reset-first-binding)))
  (xbindkey-function 'v
		     (lambda ()
		       (run-command "wmctrl -xa Gvim || gvim")
		       (reset-first-binding)))
  (xbindkey-function 'e
		     (lambda ()
		       (run-command "wmctrl -xa Emacs || emacsclient -c -n")
		       (reset-first-binding)))
  (xbindkey-function 'f
		     (lambda ()
		       (run-command "wmctrl -xa Firefox || firefox")
		       (reset-first-binding)))
  (xbindkey-function 'i
		     (lambda ()
		       (run-command "wmctrl -xa Evince || evince")
		       (reset-first-binding)))
  (xbindkey-function '(control g) reset-first-binding)
  (debug)
  (grab-all-keys))

(first-binding)
