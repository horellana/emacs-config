(use-package prettier-js
  :ensure t
  :config (progn
	    (add-hook 'web-mode-hook 'prettier-js-mode)))
