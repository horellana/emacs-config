(use-package helm
						 :ensure t
						 :config (progn
											 (helm-mode 1)
											 (global-set-key (kbd "C-h a") 'helm-apropos)
											 (global-set-key (kbd "C-x C-b") 'switch-to-buffer)))

(use-package ac-helm
						 :ensure t
						 :config (progn
											 (global-set-key (kbd "C--") 'ac-complete-with-helm)))

(provide 'helm-config)
