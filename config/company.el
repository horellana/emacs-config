(req-package company
  :ensure t
  :defer 5
  :config (eval-after-load "company"
	    '(progn
	       (setf company-minimum-prefix-length 3)
	       (setf company-show-numbers t)
	       (setf company-idle-delay 0.5)
	       (setf company-quickhelp-delay 1)

	       (global-company-mode))))
