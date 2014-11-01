(use-package flycheck
						 :ensure t
						 :commands (flycheck-mode)
						 :config (progn
											 (custom-set-variables
												'(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))

(use-package flycheck-pos-tip
						 :commands (flychek-mode)
						 :ensure t)
						 
