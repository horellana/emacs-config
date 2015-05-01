(use-package elpy
	:ensure t
	:config (progn
						(elpy-enable)))

(use-package flycheck
	:ensure t
	:defer t
	:config (progn (add-hook 'python-mode-hook 'flycheck-mode)))

(use-package virtualenvwrapper
	:ensure t
	:defer t
	:config (progn
						(venv-initialize-interactive-shells)
						(venv-initialize-eshell)))

(provide 'python-config)
