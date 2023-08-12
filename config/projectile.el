(use-package projectile
  :disabled t
  :ensure t
  :config (progn
	(eval-after-load "evil-leader"
	  '(progn
	     (evil-leader/set-key
	       "ptp" 'projectile-test-project)))))


