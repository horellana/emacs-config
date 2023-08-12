(use-package sly
  :ensure t
  :mode "\\.lisp\\'"
  :config (progn
            (setq inferior-lisp-program "/bin/sbcl")))

(eval-after-load "evil-leader"
  '(progn
     (evil-leader/set-key-for-mode 'lisp-interaction-mode "ma" 'pp-macroexpand-last-sexp)
     (evil-leader/set-key-for-mode 'lisp-mode "cl" 'slime-load-file)
     (evil-leader/set-key-for-mode 'lisp-mode "e" 'slime-eval-last-expression)
     (evil-leader/set-key-for-mode 'lisp-mode "me" 'slime-macroexpand-1)
     (evil-leader/set-key-for-mode 'lisp-mode "ma" 'slime-macroexpand-all)
     (evil-leader/set-key-for-mode 'lisp-mode "sds" 'slime-disassemble-symbol)
     (evil-leader/set-key-for-mode 'lisp-mode "sdd" 'slime-disassemble-definition)))

(add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))
