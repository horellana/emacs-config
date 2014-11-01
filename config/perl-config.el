(use-package auto-complete
						 :ensure t
						 :config (progn
											 (add-hook 'cperl-mode-hook
																 (lambda ()
																	 (setf ac-sources
																				 '(ac-source-words-in-buffer
																					 ac-source-words-in-same-mode-buffers
																					 ac-source-etags
																					 ac-source-abbrev
																					 ac-source-yasnippet))))

											 (add-hook 'cperl-mode-hook 'auto-complete-mode)))
(use-package flycheck
						 :ensure t
						 :config (progn
											 (add-hook 'cperl-mode-hook 'flycheck-mode)))
(provide 'perl-config)
