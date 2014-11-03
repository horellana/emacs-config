(defalias 'perl-mode 'cperl-mode)

(use-package auto-complete
						 :ensure t
						 :config (progn
											 (add-hook 'cperl-mode-hook
																 (lambda ()
																	 (loop for source in '(ac-source-words-in-buffer
																												 ac-source-words-in-same-mode-buffers
																												 ac-source-etags
																												 ac-source-abbrev) 
																				 do (add-to-list 'ac-sources source))))
											 
											 (use-package perl-completion
																		:ensure t
																		:config (progn
																							(add-hook 'cperl-mode-hook 
																												(lambda () 
																													(add-to-list 'ac-sources 'ac-source-perl-completion)))
																							(add-hook 'cperl-mode-hook 'perl-completion-mode)))


											 (add-hook 'cperl-mode-hook 'auto-complete-mode)))

(use-package yasnippet
						 :ensure t
						 :config (progn
											 (add-hook 'cperl-mode-hook 'yas-minor-mode)))

(use-package flycheck
						 :ensure t
						 :config (progn
											 (add-hook 'cperl-mode-hook 'flycheck-mode)))
