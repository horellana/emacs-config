(req-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :config (progn
	    (setq-default typescript-indent-level 2)))
