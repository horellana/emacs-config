(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))

(use-package slime
	:ensure t)

(use-package aggressive-indent
	:ensure t
	:config (progn
						(add-hook 'lisp-mode-hook 'aggressive-indent-mode)))

(use-package ac-slime
	:ensure t
	:config (progn
						(loop for m in '(slime-mode-hook
														 slime-repl-mode-hook)
									do (add-hook m (lambda ()
																	 (set-up-slime-ac)
																	 (auto-complete-mode))))))

(use-package evil-leader
	:ensure t
	:config (progn
						(evil-leader/set-key-for-mode 'lisp-mode "cl" 'slime-load-file)
						(evil-leader/set-key-for-mode 'lisp-mode "e" 'slime-eval-last-expression)
						(evil-leader/set-key-for-mode 'lisp-mode "me" 'slime-macroexpand-1)
						(evil-leader/set-key-for-mode 'lisp-mode "ma" 'slime-macroexpand-all)))

(provide 'common-lisp-config)
