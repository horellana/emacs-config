(use-package projectile
						 :ensure t
						 :config (progn
											 (setq projectile-indexing-method 'alien)
											 (setq projectile-enable-caching t)
											 (projectile-global-mode)))

(provide 'projectile-config)
