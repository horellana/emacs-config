(use-package projectile
						 :ensure t
						 :config (progn
											 (setq projectile-indexing-method 'alien)
											 (setq projectile-enable-caching t)
											 (projectile-global-mode)))

(use-package helm-projectile
	:ensure t)

(use-package evil-leader
	:ensure t
	:config (progn (helm-projectile-on)
								 (evil-leader/set-key-for-mode 'projectile-mode (kbd "p") 'helm-projectile)))

			
(provide 'projectile-config)
