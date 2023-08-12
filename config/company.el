(use-package company
  :disabled t
  :ensure t
  :defer 5
  :config (eval-after-load "company"
	    '(progn
	       (setf company-minimum-prefix-length 1)
	       (setf company-show-numbers nil)
	       (setf company-idle-delay 0.5)
	       (setf company-quickhelp-delay 0.1)
	       (setq company-backends '((company-capf company-dabbrev-code)))

	       (add-hook 'css-mode-hook
			 (lambda ()
			   (setq-local company-backends '((company-css company-capf)))))

	       (global-company-mode))))

(use-package company-postframe
  :ensure t
  :disabled t
  :config (progn
	    (company-postframe-mode 1)))
