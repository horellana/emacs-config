(setq inferior-lisp-program "/home/juiko/git/sbcl/run-sbcl.sh")
(setq slime-contribs '(slime-fancy slime-company))

(use-package evil-leader
	:ensure t
	:config (progn
						(add-hook 'lisp-mode-hook 'evil-leader-mode)
						(evil-leader/set-key-for-mode 'lisp-mode "cl" 'slime-load-file)
						(evil-leader/set-key-for-mode 'lisp-mode "e" 'slime-eval-last-expression)
						(evil-leader/set-key-for-mode 'lisp-mode "me" 'slime-macroexpand-1)
						(evil-leader/set-key-for-mode 'lisp-mode "ma" 'slime-macroexpand-all)))

(use-package aggressive-indent
	:ensure t
	:config (progn (add-hook 'lisp-mode-hook 
													 (lambda () (aggressive-indent-mode t)))))

(use-package paredit
	:ensure t
	:config (progn (add-hook 'lisp-mode-hook 
													 (lambda () (paredit-mode t)))))

(use-package evil-paredit
	:ensure t
	:config (progn (add-hook 'lisp-mode-hook 
													 (lambda () (evil-paredit-mode t)))))

(use-package evil-lisp-state
	:ensure t)

(use-package slime
	:ensure t
	:config (progn
						(add-hook 'lisp-mode-hook
											(lambda ()
												(unless (slime-connected-p)
													(save-excursion (slime)))))))

(use-package slime-company
	:ensure t)

(provide 'common-lisp-config)
