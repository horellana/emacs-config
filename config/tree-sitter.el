(use-package tree-sitter
  :ensure t
  :config (progn
	    (global-tree-sitter-mode)
	    (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :after (tree-sitter)
  :ensure t)
