(req-package smart-jump
  :ensure t
  :bind (("M-." . smart-jump-go)
	 ("M-," . smart-jump-back)
	 ("M-?" . smart-jump-references)))
