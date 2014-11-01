(use-package pos-tip
						 :ensure t)

(use-package auto-complete
						 :ensure t
						 :config
						 (progn
							 (custom-set-variables
								'(ac-dwim t)
								'(ac-use-fuzzy t)
								'(ac-auto-start t)
								'(ac-candidate-limit 250)
								'(ac-delay 0.5)
								'(ac-auto-start 3))))

(use-package ac-etags
						 :ensure t
						 :config (progn (ac-etags-setup)))

(provide 'myac-config)
