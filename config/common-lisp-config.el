(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy slime-company))

(use-package slime-company
	:ensure t)

(use-package slime
	:ensure t)

(use-package aggressive-indent
	:ensure t
	:config (progn
						(add-hook 'lisp-mode-hook 'aggressive-indent-mode)))

(use-package evil-leader
	:ensure t
	:config (progn
						(evil-leader/set-key-for-mode 'lisp-mode "cl" 'slime-load-file)
						(evil-leader/set-key-for-mode 'lisp-mode "e" 'slime-eval-last-expression)
						(evil-leader/set-key-for-mode 'lisp-mode "me" 'slime-macroexpand-1)
						(evil-leader/set-key-for-mode 'lisp-mode "ma" 'slime-macroexpand-all)))

(provide 'common-lisp-config)
