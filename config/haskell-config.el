(use-package haskell-mode
  :ensure t
  :config (progn
						(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
						(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
						
						(define-key haskell-mode-map (kbd "C-x C-d") nil)
						(define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
						(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
						(define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
						(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
						(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
						(define-key haskell-mode-map (kbd "C-c M-.") nil)
						(define-key haskell-mode-map (kbd "C-c C-d") nil)))

(use-package flycheck
  :ensure t
  :config (progn
						(add-hook 'haskell-mode-hook 'flycheck-mode)))

(use-package flycheck-haskell
  :ensure t
  :config (progn
						(add-hook 'flycheck-mode 'flycheck-haskell-setup)))

(provide 'haskell-config)
