(use-package jde
	:disabled t
	:ensure t
  :commands java-mode
  :config (progn
            (use-package imenu)
            (use-package abbrev)
            (add-hook 'jde-mode-hook 'jde-abbrev-mode)
            (add-hook 'jde-mode-hook 'imenu-tree)
            (add-hook 'java-mode-hook 'my-java-config-hook)))

(provide 'java-config)
