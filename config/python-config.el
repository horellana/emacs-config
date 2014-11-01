(use-package anaconda-mode 
						 :ensure t
						 :config (progn
											 (add-hook 'python-mode-hook 'anaconda-mode)
											 (add-hook 'python-mode-hook 'eldoc-mode)))

(use-package company-anaconda
						 :ensure t
						 :config (progn
											 (add-to-list 'company-backends 'company-anaconda)
											 (add-hook 'python-mode-hook 'company-mode)))

(use-package flycheck
						 :ensure t
             :commands python-mode
             :config (progn (add-hook 'python-mode-hook 'flycheck-mode)))

(use-package aggressive-indent
						 :ensure t
						 :config (progn
											 (add-hook 'python-mode-hook 
																 (lambda () (aggresive-indent-mode 0)))))

(provide 'python-config)
