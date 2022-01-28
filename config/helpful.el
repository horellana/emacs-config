(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 ("C-c C-d" . helpful-at-point)
	 ("C-h F" . helpful-function))

  :config (progn
	    (with-eval-after-load "counsel"
	      (setq counsel-describe-function-function #'helpful-callable)
	      (setq counsel-describe-variable-function #'helpful-variable))))
