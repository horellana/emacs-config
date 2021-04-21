(eval-after-load "evil-lisp-state"
  '(progn
     (evil-leader/set-key-for-mode 'cider-mode "e" 'cider-eval-last-sexp)))
