(use-package flycheck
						 :ensure t
						 :config (progn
											 (add-hook 'emacs-lisp-mode-hook
																 (lambda () (flycheck-mode)))))

(use-package eldoc
						 :ensure t
						 :config (progn
											 (add-hook 'emacs-lisp-mode-hook
																 (lambda () (eldoc-mode)))))

(use-package paredit
						 :ensure t
						 :config (progn
											 (add-hook 'emacs-lisp-mode-hook
																 (lambda () (paredit-mode)))))

(use-package aggressive-indent
						 :ensure t
						 :config (progn
											 (add-hook 'emacs-lisp-mode-hook 
																 'aggressive-indent-mode)))

(use-package company
						 :ensure t
						 :config (progn
											 (add-hook 'emacs-lisp-mode-hook 'company-mode)
											 (add-hook 'inferior-emacs-lisp-mode-hook 'company-mode)))

(add-hook 'emacs-lisp-mode-hook
					(lambda () (add-hook 'local-write-file-hooks 'check-parens)))

(add-hook 'emacs-lisp-mode-hook
					(lambda ()
						(define-key emacs-lisp-mode-map (kbd "C-h C-f")
							(lambda ()
								(interactive)
								(find-function (symbol-at-point))))))

(add-hook 'emacs-lisp-mode-hook
					(lambda ()
						(show-paren-mode)))

(provide 'elisp-config)
