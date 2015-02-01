(use-package company
	:ensure t
	:defer t
	:config (progn
						(setf company-idle-delay nil)))

(use-package helm-company
	:ensure t
	:config (progn
						(define-key company-mode-map (kbd "C--") 'helm-company)
						(define-key company-active-map (kbd "C--") 'helm-company)))

(provide 'company-config)
