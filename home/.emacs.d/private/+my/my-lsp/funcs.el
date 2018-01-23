(defun my-lsp//sync-peek-face ()
  (set-face-attribute 'lsp-ui-peek-list nil :background (face-attribute 'hl-line :background))
  (set-face-attribute 'lsp-ui-peek-peek nil :background (face-attribute 'hl-line :background))
  (set-face-attribute 'lsp-ui-peek-selection nil
                      :background (face-attribute 'highlight :background)
                      :foreground (face-attribute 'default :foreground))
  (set-face-attribute 'lsp-ui-peek-filename nil
                      :foreground (face-attribute 'font-lock-constant-face :foreground))
  (set-face-attribute 'lsp-ui-peek-highlight nil
                      :background (face-attribute 'highlight :background)
                      :foreground (face-attribute 'highlight :foreground)
                      :distant-foreground (face-attribute 'highlight :foreground))
  (set-face-attribute 'lsp-ui-peek-header nil
                      :background (face-attribute 'highlight :background)
                      :foreground (face-attribute 'default :foreground))
  )
