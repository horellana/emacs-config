(use-package flycheck
						 :config (progn
											 (add-hook 'haskell-mode-hook 'flycheck-mode)))
(provide 'haskell-config)
