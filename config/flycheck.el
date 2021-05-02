(use-package flycheck
  :ensure t
  :defer t
  :config (progn
	    (custom-set-faces
	     '(flycheck-error ((t (:underline "Red1"))))
	     '(flycheck-info ((t (:underline "ForestGreen"))))
	     '(flycheck-warning ((t (:underline "DarkOrange")))))

	    (global-flycheck-mode)))

(use-package flycheck-package
  :ensure t
  :requires flycheck)
