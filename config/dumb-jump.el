(use-package dumb-jump
  :ensure t
  :config (progn
	    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
	    (setq xref-show-definitions-function #'xref-show-definitions-completing-read)))
