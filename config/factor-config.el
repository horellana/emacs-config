(use-package factor-mode
						 :commands (factor-mode fuel-mode)
						 :init (progn
										 (add-to-list 'load-path
																	"/home/juiko/git/factor/misc/fuel/"))
						 :config (progn
											 (let ((root-dir "/home/juiko/git/factor"))
												 (setf factor-root-dir root-dir
															 fuel-listener-factor-binary (concat factor-root-dir
																																	 "/"
																																	 "factor")
															 
															 fuel-listener-factor-image (concat factor-root-dir
																																	"/"
																																	"factor.image")

															 factor-indent-level 2))

											 (use-package company
																		:ensure t
																		:config (progn 
																							(add-hook 'factor-mode-hook 
																												'company-mode)))
											 (use-package fuel-mode
																		:config (progn
																							(add-hook 'factor-mode-hook 
																												'fuel-mode)))
											 
											 (add-hook 'factor-mode-hook 'show-paren-mode)))
(use-package evil
						 :commands (factor-mode fuel-mode run-factor)
						 :ensure t
						 :config (progn
											 (evil-set-initial-state 'fuel-listener-mode 'emacs)))

(provide 'factor-config)
