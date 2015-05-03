(use-package helm
	:config (progn
						(helm-mode t)
						(global-set-key (kbd "C-h a") 'helm-apropos)
						(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

						(use-package helm-swoop
							:ensure t
							:commands helm-swoop)

						(use-package helm-ag
							:ensure t
							:commands helm-ag)

						(use-package helm-ack
							:ensure t
							:commands helm-ack)

						(use-package ac-helm
							:ensure t
							:commands ac-helm
							:init (ignore-errors
											(define-key ac-mode-map (kbd "C--") 'ac-complete-with-helm)))
						(add-to-list 'display-buffer-alist
												 `(,(rx bos "*helm" (* not-newline) "*" eos)
													 (display-buffer-in-side-window)
													 (inhibit-same-window . t)
													 (window-height . 0.4)))))

(provide 'my-helm-config)
