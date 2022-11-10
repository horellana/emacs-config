(use-package flycheck
  :ensure t
  :disabled t
  :commands (flycheck-mode)
  :config (progn
	    (custom-set-faces
	     '(flycheck-error ((t (:underline "Red1"))))
	     '(flycheck-info ((t (:underline "ForestGreen"))))
	     '(flycheck-warning ((t (:underline "DarkOrange")))))))

(use-package flycheck-package
  :ensure t
  :after (flycheck)
  :commands (flycheck-mode))
