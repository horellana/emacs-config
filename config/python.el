(req-package pipenv
  :disabled t
  :ensure t
  :hook (python-mode . pipenv-mode)
  :config (progn
	    (defun pipenv-shell ()
	      (message "(pipenv-shell) STUB"))
	    (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended)))
