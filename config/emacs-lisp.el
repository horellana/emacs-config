(eval-after-load "evil-leader"
  '(progn
     (evil-leader/set-key-for-mode 'emacs-lisp-mode "ma" 'pp-macroexpand-last-sexp)))
