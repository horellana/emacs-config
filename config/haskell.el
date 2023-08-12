(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'")

(use-package lsp-haskell
  :ensure t
  :commands (haskell-mode)
  :config (progn
	    (add-hook 'haskell-mode-hook 'lsp)))

(eval-after-load "evil-lisp-state"
  '(progn
     (evil-leader/set-key-for-mode 'haskell-mode "H" 'haskell-hoogle)))
