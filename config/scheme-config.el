(use-package geiser
						 :ensure t
						 :config (progn
											 (add-hook 'geiser-mode 'geiser-autodoc-mode)
											 (add-hook 'scheme-mode 'turn-on-geiser-mode)))

(use-package company
						 :ensure t
						 :config (progn
											 (add-hook 'geiser-mode 
																 (lambda () (company-mode t)))))

(use-package evil
						 :ensure t
						 :config (progn
											 (evil-set-initial-state 'geiser-repl-mode 'emacs)))

(provide 'scheme-config)
