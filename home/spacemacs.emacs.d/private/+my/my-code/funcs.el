(defun cquery/base () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/base"))
(defun cquery/callers () (interactive) (lsp-ui-peek-find-custom 'callers "$cquery/callers"))
(defun cquery/derived () (interactive) (lsp-ui-peek-find-custom 'derived "$cquery/derived"))
(defun cquery/vars () (interactive) (lsp-ui-peek-find-custom 'vars "$cquery/vars"))
(defun cquery/random () (interactive) (lsp-ui-peek-find-custom 'random "$cquery/random"))
(defun text-document/type-definition () (interactive) (lsp-ui-peek-find-custom 'type "textDocument/typeDefinition"))

(defun cquery/references-address ()
  (interactive)
  (lsp-ui-peek-find-custom
   'address "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context
              '(:role 128))))

(defun cquery/references-read ()
  (interactive)
  (lsp-ui-peek-find-custom
   'read "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context
              '(:role 8))))

(defun cquery/references-write ()
  (interactive)
  (lsp-ui-peek-find-custom
   'write "textDocument/references"
   (plist-put (lsp--text-document-position-params) :context
              '(:role 16))))

(defvar +amos/default-include-headers
  '("algorithm" "any" "array" "atomic" "bitset" "cassert" "ccomplex" "cctype" "cerrno"
    "cfenv" "cfloat" "chrono" "cinttypes" "ciso646" "climits" "clocale" "cmath" "codecvt"
    "complex" "complex.h" "condition_variable" "csetjmp" "csignal" "cstdalign" "cstdarg"
    "cstdbool" "cstddef" "cstdint" "cstdio" "cstdlib" "cstring" "ctgmath" "ctime" "cuchar"
    "cwchar" "cwctype" "cxxabi.h" "deque" "exception" "fenv.h" "forward_list" "fstream"
    "functional" "future" "initializer_list" "iomanip" "ios" "iosfwd" "iostream" "istream"
    "iterator" "limits" "list" "locale" "map" "math.h" "memory" "mutex" "new" "numeric"
    "optional" "ostream" "queue" "random" "ratio" "regex" "scoped_allocator" "set"
    "shared_mutex" "sstream" "stack" "stdexcept" "stdlib.h" "streambuf" "string" "string_view"
    "system_error" "tgmath.h" "thread" "tuple" "type_traits" "typeindex" "typeinfo" "unordered_map"
    "unordered_set" "utility" "valarray" "variant" "vector" "auto_ptr.h" "backward_warning.h"
    "binders.h" "hash_fun.h" "hash_map" "hash_set" "hashtable.h" "strstream" "adxintrin.h"
    "altivec.h" "ammintrin.h" "arm_acle.h" "arm_neon.h" "armintr.h" "avx2intrin.h" "avx512bwintrin.h"
    "avx512cdintrin.h" "avx512dqintrin.h" "avx512erintrin.h" "avx512fintrin.h" "avx512ifmaintrin.h"
    "avx512ifmavlintrin.h" "avx512pfintrin.h" "avx512vbmiintrin.h" "avx512vbmivlintrin.h"
    "avx512vlbwintrin.h" "avx512vlcdintrin.h" "avx512vldqintrin.h" "avx512vlintrin.h" "avx512vpopcntdqintrin.h"
    "avxintrin.h" "bmi2intrin.h" "bmiintrin.h" "clflushoptintrin.h" "clzerointrin.h" "cpuid.h"
    "cuda_wrappers" "emmintrin.h" "f16cintrin.h" "fcntl.h" "float.h" "fma4intrin.h" "fmaintrin.h" "fxsrintrin.h"
    "htmintrin.h" "htmxlintrin.h" "ia32intrin.h" "immintrin.h" "intrin.h" "inttypes.h" "iso646.h"
    "limits.h" "lwpintrin.h" "lzcntintrin.h" "mm3dnow.h" "mm_malloc.h" "mmintrin.h" "module.modulemap"
    "msa.h" "mwaitxintrin.h" "nmmintrin.h" "opencl-c.h" "pkuintrin.h" "pmmintrin.h" "popcntintrin.h"
    "prfchwintrin.h" "rdseedintrin.h" "rtmintrin.h" "s390intrin.h" "sanitizer" "shaintrin.h" "smmintrin.h"
    "stdalign.h" "stdarg.h" "stdatomic.h" "stdbool.h" "stddef.h" "stdint.h" "stdnoreturn.h" "tbmintrin.h"
    "tgmath.h" "tmmintrin.h" "unwind.h" "vadefs.h" "varargs.h" "vecintrin.h" "wmmintrin.h" "x86intrin.h"
    "xmmintrin.h" "xopintrin.h" "xray" "xsavecintrin.h" "xsaveintrin.h" "xsaveoptintrin.h" "xsavesintrin.h"
    "xtestintrin.h" "unistd.h" "libaio.h" "numa.h"))

(defun my//insert-include (h &rest others)
  "Add an #include line for `h' near top of file, avoiding duplicates."
  (interactive "M#include: ")
  (dolist (header (cons h others))
    (let ((incl (if (string-match-p "\\." header)
                    (format "#include \"%s\"" header)
                  (format "#include <%s>" header))))
      (save-excursion
        (unless (search-backward incl nil t)
          (when (search-backward "#include" nil t)
            (forward-line)
            (beginning-of-line))
          (insert incl)
          (newline))))))

(defun my/ivy-insert-include ()
  (interactive)
  (ivy-read "#include: "
            (append
             +amos/default-include-headers
             (split-string
              (shell-command-to-string "(cd /usr/local/include ; find . -type f ; cd /usr/include ; find -L sys -type f) | sed 's=^./=='")))
            :action #'my//insert-include))


;; eshell

(with-eval-after-load 'em-prompt
  (defun eshell-previous-prompt (n)
    "Move to end of Nth previous prompt in the buffer.
See `eshell-prompt-regexp'."
    (interactive "p")
    (beginning-of-line)               ; Don't count prompt on current line.
    ;; PATCH beginning-of-line does not move across the prompt
    (backward-char)
    (eshell-next-prompt (- n))))

;; PATCH counsel-esh-history
(defun my/ivy-eshell-history ()
  (interactive)
  (require 'em-hist)
  (let* ((start-pos (save-excursion (eshell-bol) (point)))
         (end-pos (point))
         (input (buffer-substring-no-properties start-pos end-pos))
         (command (ivy-read "Command: "
                            (delete-dups
                             (when (> (ring-size eshell-history-ring) 0)
                               (ring-elements eshell-history-ring)))
                            :initial-input input)))
    (setf (buffer-substring start-pos end-pos) command)
    (end-of-line)))

(defun +eshell/quit-or-delete-char (arg)
  (interactive "p")
  (if (and (eolp) (looking-back eshell-prompt-regexp nil))
      (eshell-life-is-too-much)
    (delete-char arg)))

;; PATCH no git
(defun +eshell-prompt ()
  (concat (propertize (abbreviate-file-name (eshell/pwd)) 'face 'eshell-prompt)
          (propertize " λ " 'face 'font-lock-constant-face)))

(defun eshell/l (&rest args) (eshell/ls "-l" args))
(defun eshell/e (file) (find-file file))
(defun eshell/md (dir) (eshell/mkdir dir) (eshell/cd dir))
(defun eshell/ft (&optional arg) (treemacs arg))

(defun eshell/up (&optional pattern)
  (let ((p (locate-dominating-file
                (f-parent default-directory)
                (lambda (p)
                  (if pattern
                      (string-match-p pattern (f-base p))
                    t)))
))
    (eshell/pushd p)))


(defun my/ffap ()
  (interactive)
  (-if-let ((filename (ffap-guess-file-name-at-point)))
      (ffap filename)
    (user-error "No file at point")))

(defun my/compilation-buffer ()
  (interactive)
  (set-window-buffer nil (get-buffer "*compilation*")))

(defun my-projectile/dotemacs-elpa-package (dir)
  (when (string-match ".*emacs.d/elpa/[^/]+/develop/[^/]+/" dir)
    (match-string 0 dir)))

(defun my-avy/goto-paren ()
  (interactive)
  (avy--generic-jump "(" nil 'pre))

(defun my-avy/goto-conditional ()
  (interactive)
  (avy--generic-jump "\\s(\\(if\\|cond\\|when\\|unless\\)\\b" nil 'pre))

(defun my/define-key (keymap key def &rest bindings)
  "Define multi keybind with KEYMAP KEY DEF BINDINGS."
  (interactive)
  (while key
    (define-key keymap (kbd key) def)
    (setq key (pop bindings)
          def (pop bindings))))


;;; realgud

(defun my/realgud-eval-nth-name-forward (n)
  (interactive "p")
  (save-excursion
    (let (name)
      (while (and (> n 0) (< (point) (point-max)))
        (let ((p (point)))
          (if (not (c-forward-name))
              (progn
                (c-forward-token-2)
                (when (= (point) p) (forward-char 1)))
            (setq name (buffer-substring-no-properties p (point)))
            (cl-decf n 1))))
      (when name
        (realgud:cmd-eval name)
        nil))))

(defun my/realgud-eval-nth-name-backward (n)
  (interactive "p")
  (save-excursion
    (let (name)
      (while (and (> n 0) (> (point) (point-min)))
        (let ((p (point)))
          (c-backward-token-2)
          (when (= (point) p) (backward-char 1))
          (setq p (point))
          (when (c-forward-name)
            (setq name (buffer-substring-no-properties p (point)))
            (goto-char p)
            (cl-decf n 1))))
      (when name
        (realgud:cmd-eval name)
        nil))))

(defun my/realgud-eval-region-or-word-at-point ()
  (interactive)
  (when-let*
      ((cmdbuf (realgud-get-cmdbuf))
       (process (get-buffer-process cmdbuf))
       (expr
        (if (evil-visual-state-p)
            (let ((range (evil-visual-range)))
              (buffer-substring-no-properties (evil-range-beginning range)
                                              (evil-range-end range)))
          (word-at-point)
          )))
    (with-current-buffer cmdbuf
	    (setq realgud:process-filter-save (process-filter process))
	    (set-process-filter process 'realgud:eval-process-output))
    (realgud:cmd-eval expr)
    ))


;;; elisp

(defun my/realtime-elisp-doc-function ()
  (let ((w (selected-window)))
    (when-let* ((s (intern-soft (current-word))))
      (cond
       ((fboundp s) (describe-function s))
       ((boundp s) (describe-variable s))
       )
      (select-window w)
      nil)))

(defun my/realtime-elisp-doc ()
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
    (if (advice-function-member-p #'my/realtime-elisp-doc-function eldoc-documentation-function)
        (remove-function (local 'eldoc-documentation-function) #'my/realtime-elisp-doc-function)
      (add-function :after-while (local 'eldoc-documentation-function) #'my/realtime-elisp-doc-function))))

(defun my/expand-line ()
  (interactive)
  (let ((hippie-expand-try-functions-list
         '(try-expand-line)))
    (call-interactively 'hippie-expand)))


;;; xref

(defun my-advice/xref-set-jump (&optional args)
  (lsp-ui-peek--with-evil-jumps (evil-set-jump)))

(defun my-xref/find-definitions ()
  (interactive)
  (if lsp-mode (lsp-ui-peek-find-definitions) (spacemacs/jump-to-definition)))

(defun my-xref/find-references ()
  (interactive)
  (if lsp-mode
      (lsp-ui-peek-find-references)
    (spacemacs/search-project-rg-region-or-symbol)))

;;; Override
;; This function is transitively called by xref-find-{definitions,references,apropos}
(require 'xref)
(defun xref--show-xrefs (xrefs display-action &optional always-show-list)
  (cond
   ((cl-some (lambda (x) (string-match-p x buffer-file-name))
             my-xref-blacklist)
    nil)
   (t
    ;; PATCH
    (lsp-ui-peek--with-evil-jumps (evil-set-jump))

    ;; PATCH Jump to the first candidate
    (if (not (cdr xrefs))
        (xref--pop-to-location (car xrefs) display-action)
      (funcall xref-show-xrefs-function xrefs
               `((window . ,(selected-window))))
      ))))

(with-eval-after-load 'ivy-xref
  (defun ivy-xref-show-xrefs (xrefs alist)
    (let (ivy-display-function)
     (minibuffer-with-setup-hook #'hydra-ivy/body
       (minibuffer-with-setup-hook #'ivy-toggle-calling
         (ivy-read "xref: " (ivy-xref-make-collection xrefs)
                   :require-match t
                   :action #'(lambda (candidate)
                               (xref--show-location (cdr candidate) 'quit))))))))


;; https://github.com/syl20bnr/spacemacs/pull/9911

(defmacro spacemacs|define-reference-handlers (mode &rest handlers)
  "Defines reference handlers for the given MODE.
This defines a variable `spacemacs-reference-handlers-MODE' to which
handlers can be added, and a function added to MODE-hook which
sets `spacemacs-reference-handlers' in buffers of that mode."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "spacemacs//init-reference-handlers-%S" mode)))
        (handlers-list (intern (format "spacemacs-reference-handlers-%S" mode))))
    `(progn
       (defvar ,handlers-list ',handlers
         ,(format (concat "List of mode-specific reference handlers for %S. "
                          "These take priority over those in "
                          "`spacemacs-default-reference-handlers'.")
                  mode))
       (defun ,func ()
         (setq spacemacs-reference-handlers
               (append ,handlers-list
                       spacemacs-default-reference-handlers)))
       (add-hook ',mode-hook ',func)
       (with-eval-after-load 'bind-map
         (spacemacs/set-leader-keys-for-major-mode ',mode
           "gr" 'spacemacs/jump-to-reference)))))

(defun spacemacs/jump-to-reference ()
  "Jump to reference around point using the best tool for this action."
  (interactive)
  (catch 'done
    (let ((old-window (selected-window))
          (old-buffer (current-buffer))
          (old-point (point)))
      (dolist (-handler spacemacs-reference-handlers)
        (let ((handler (if (listp -handler) (car -handler) -handler))
              (async (when (listp -handler)
                       (plist-get (cdr -handler) :async))))
          (ignore-errors
            (call-interactively handler))
          (when (or (eq async t)
                    (and (fboundp async) (funcall async))
                    (not (eq old-point (point)))
                    (not (equal old-buffer (window-buffer old-window))))
            (throw 'done t)))))
    (message "No reference handler was able to find this symbol.")))

(defun spacemacs/jump-to-reference-other-window ()
  "Jump to reference around point in other window."
  (interactive)
  (let ((pos (point)))
    ;; since `spacemacs/jump-to-reference' can be asynchronous we cannot use
    ;; `save-excursion' here, so we have to bear with the jumpy behavior.
    (switch-to-buffer-other-window (current-buffer))
    (goto-char pos)
    (spacemacs/jump-to-reference)))


;; dumb-jump

(defun my-advice/dumb-jump-go (orig-fun &rest args)
  (unless (or lsp-mode
              (cl-some
               (lambda (x) (string-match-p x buffer-file-name))
               my-xref-blacklist))
    (apply orig-fun args)))
