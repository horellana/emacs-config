(use-package paredit
						 :ensure t)
(use-package auto-complete
						 :ensure t)
(use-package highlight-stages
						 :ensure t)

(add-hook 'lisp-mode-hook 'paredit-mode)

(add-hook 'lisp-mode-hook 'paren-face-mode)

(add-hook 'lisp-mode-hook
          (lambda ()
            (slime-mode t)
            (abbrev-mode t)
            (electric-pair-mode 1)
            (show-paren-mode t)))

(add-hook 'lisp-mode-hook 'highlight-stages-mode)

(add-to-list 'load-path "/home/juiko/git/slime/")
(add-to-list 'load-path "/home/juiko/git/helm-slime/")

(use-package slime-autoloads
						 :commands (slime-mode lisp-mode)
						 :config (progn
											 (defun quickdoc-lib (lib)
												 (interactive "Slib name: ")
												 (browse-url (format "http://quickdocs.org/%s" lib)))

											 (defun quickdoc-api (lib)
												 (interactive "Slib name: ")
												 (browse-url (format "http://quickdocs.org/%s/api" lib)))

											 (setq slime-contribs '(slime-fancy
																							slime-repl
																							slime-indentation
																							slime-sbcl-exts
																							slime-autodoc))
											 
											 (slime-setup)

											 (setq lisp-indent-function 'common-lisp-indent-function)

											 (setq inferior-lisp-program "/usr/bin/sbcl")

											 (add-hook 'slime-mode-hook
																 (lambda ()
																	 (define-key slime-mode-map (kbd "C-c C-d C-l") 'slime-hyperspec-lookup)
																	 (define-key slime-repl-mode-map (kbd "C-c C-d C-l") 'slime-hyperspec-lookup)))

											 (add-hook 'slime-repl-mode-hook
																 (lambda ()
																	 (abbrev-mode t)
																	 (electric-pair-mode t)
																	 (show-paren-mode t)))
											 (add-hook 'slime-mode-hook 
																 (lambda ()
																	 (unless (slime-connected-p)
																		 (save-excursion (slime)))))))

(use-package evil
						 :ensure t
						 :config (progn
											 (evil-leader/set-key-for-mode 
												'lisp-mode "e" 'slime-eval-last-expression)))

(use-package auto-complete
						 :ensure t
						 :config (progn
											 (add-hook 'slime-repl-mode-hook 'auto-complete-mode)
											 (add-hook 'lisp-mode-hook 'auto-complete-mode)))

(use-package ac-slime
						 :ensure t
						 :config (progn
											 (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
											 (add-hook 'lisp-mode-hook 'set-up-slime-ac)))

(use-package aggressive-indent
						 :ensure t
						 :commands (lisp-mode slime-mode slime)
						 :config (progn
											 (add-hook 'lisp-mode-hook 'aggressive-indent-mode)))

(provide 'common-lisp-config)

