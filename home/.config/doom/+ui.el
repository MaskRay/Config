;;; private/my/+ui.el -*- lexical-binding: t; -*-

(setq
 doom-font (font-spec :family "Monospace" :size 25)
 doom-variable-pitch-font (font-spec :family "Monospace")
 doom-unicode-font (font-spec :family "Monospace")
 )
(setq doom-big-font (font-spec :family "Monospace" :size 27))
(remove-hook 'doom-init-ui-hook #'blink-cursor-mode)

;; (setq doom-theme 'doom-nord)
