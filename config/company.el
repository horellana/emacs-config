(use-package company
  :ensure t
  :defer 5
  :config (eval-after-load "company"
	    '(progn
	       (setf company-minimum-prefix-length 3)
	       (setf company-show-numbers t)
	       (setf company-idle-delay 0.5)
	       (setf company-quickhelp-delay 1)

	       (setq company-backends '(company-etags (company-dabbrev-code company-capf)))

	       (global-company-mode))))

(use-package company-postframe
  :ensure t
  :disabled t
  :config (progn
	    (company-postframe-mode 1)))
