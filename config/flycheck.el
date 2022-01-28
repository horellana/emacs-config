(use-package flycheck
  :ensure t
  :commands (flycheck-mode)
  :config (progn
	    (custom-set-faces
	     '(flycheck-error ((t (:underline "Red1"))))
	     '(flycheck-info ((t (:underline "ForestGreen"))))
	     '(flycheck-warning ((t (:underline "DarkOrange")))))))

(use-package flycheck-package
  :ensure t
  :commands (flycheck-mode))
