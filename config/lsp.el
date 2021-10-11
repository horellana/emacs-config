(use-package lsp-mode
  :ensure t
  :config (progn
	    (setq lsp-ui-doc-enable nil)
	    (setq lsp-idle-delay 0.500)

	    (add-hook 'c-mode-hook 'lsp)
	    (add-hook 'c++-mode-hook 'lsp)

	    (with-eval-after-load "yasnippet"
	      (add-hook 'lsp-mode-hook 'yas-minor-mode))
	    ))

(use-package lsp-ui
  :requires (lsp-mode)
  :ensure t)

(use-package lsp-ivy
  :requires (lsp-mode)
  :ensure t)

