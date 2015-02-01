(add-hook 'c-mode-common-hook 'company-mode)
(add-hook 'c-mode-common-hook 'flycheck-mode)
(add-hook 'c-mode-common-hook 'aggressive-indent-mode)

(use-package ggtags
	:ensure t
	:config (progn
						(add-hook 'c-mode-common-hook 'ggtags-mode)))

(use-package yasnippet
	:ensure t
	:config (progn
						(add-hook 'c-mode-common-hook 'yas/minor-mode-on)))

(use-package company-c-headers
	:ensure t
	:config (progn
						(add-to-list 'company-backends 'company-c-headers)))

(use-package irony
	:ensure t
	:config (progn
						(add-hook 'c++-mode-hook 'irony-mode)
						(add-hook 'c-mode-hook 'irony-mode)
						
						;; replace the `completion-at-point' and `complete-symbol' bindings in
						;; irony-mode's buffers by irony-mode's function
						(defun my-irony-mode-hook ()
							(define-key irony-mode-map [remap completion-at-point]
								'irony-completion-at-point-async)
							(define-key irony-mode-map [remap complete-symbol]
								'irony-completion-at-point-async))
						(add-hook 'irony-mode-hook 'my-irony-mode-hook)
						(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(use-package company-irony
	:ensure t
	:config (progn
						(add-to-list 'company-backends 'company-irony)))

(provide 'cpp-config)
