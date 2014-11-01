(use-package undo-tree
						 :defer t
						 :config (progn
											 (global-set-key (kbd "C-x u") 'undo-tree-visualize)))

(provide 'undo-tree-config)
