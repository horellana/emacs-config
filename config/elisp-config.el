(use-package flycheck
	:config (progn
						(add-hook 'emacs-lisp-mode-hook
											(lambda () (flycheck-mode)))))

(use-package eldoc
	:config (progn
						(add-hook 'emacs-lisp-mode-hook
											(lambda () (eldoc-mode)))))

(use-package paredit
	:config (progn
						(add-hook 'emacs-lisp-mode-hook
											(lambda () (paredit-mode)))))

(add-hook 'emacs-lisp-mode-hook
					(lambda () (add-hook 'local-write-file-hooks 'check-parens)))

(add-hook 'emacs-lisp-mode-hook
					(lambda ()
						(make-local-variable 'ac-sources)
						(setq-local ac-sources
												'(ac-source-variables
													ac-source-words-in-same-mode-buffers
													ac-source-words-in-buffer
													ac-source-functions
													ac-source-features
													ac-source-abbrev
													ac-source-symbols))))

(add-hook 'inferior-emacs-lisp-mode-hook
					(lambda ()
						(make-local-variable 'ac-sources)
						(setq-local ac-sources
												'(ac-source-variables
													ac-source-words-in-same-mode-buffers
													ac-source-words-in-buffer
													ac-source-functions
													ac-source-features
													ac-source-abbrev
													ac-source-symbols))))

(add-hook 'inferior-emacs-lisp-mode 'company-mode)

(add-hook 'emacs-lisp-mode-hook
					(lambda ()
						(define-key emacs-lisp-mode-map (kbd "C-h C-f")
							(lambda ()
								(interactive)
								(find-function (symbol-at-point))))))

(add-hook 'emacs-lisp-mode-hook
					(lambda ()
						(electric-pair-mode)
						(show-paren-mode)))

(add-hook 'emacs-lisp-mode-hook
					(lambda ()
						(use-package company
								:config (progn
													(company-mode)))))

(provide 'elisp-config)
