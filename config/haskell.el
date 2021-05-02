(req-package haskell-mode
  :ensure t
  :mode "\\.hs\\'")

(eval-after-load "evil-lisp-state"
  '(progn
     (evil-leader/set-key-for-mode 'haskell-mode "H" 'haskell-hoogle)))
