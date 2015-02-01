(add-to-list 'load-path
						 "/home/juiko/git/factor/misc/fuel/")

(use-package factor-mode
						 :config (progn
											 (let ((root-dir "/home/juiko/git/factor"))
												 (setf factor-root-dir root-dir
															 fuel-listener-factor-binary (concat factor-root-dir
																																	 "/"
																																	 "factor")
															 
															 fuel-listener-factor-image (concat factor-root-dir
																																	"/"
																																	"factor.image")

															 factor-indent-level 2))

											 (use-package company
																		:ensure t
																		:config (progn 
																							(add-hook 'factor-mode-hook 
																												'company-mode)))
											 (use-package fuel-mode
																		:config (progn
																							(add-hook 'factor-mode-hook 
																												'fuel-mode)))
											 
											 (add-hook 'factor-mode-hook 'show-paren-mode)))

(use-package evil
						 :commands (factor-mode fuel-mode run-factor)
						 :ensure t
						 :config (progn
											 (evil-set-initial-state 'fuel-listener-mode 'emacs)
											 (evil-set-initial-state 'fuel-debug-mode 'emacs)))

(use-package evil-leader
						 :ensure t
						 :config (progn
											 (evil-leader/set-key-for-mode 'factor-mode "l"
																										 'fuel-run-file)
											 (evil-leader/set-key-for-mode 'factor-mode "z"
																										 'run-factor)))

(defun fuel/complete (prefix)
	(let ((raw-results (fuel-completion--complete prefix nil)))
		(-flatten (cons (first raw-results) (cons (rest raw-results) nil)))))

(defun fuel/complete-at-point-function ()
	(fuel/complete (factor-symbol-at-point)))

(defun company-fuel-backend (command &optional arg &rest ignored)
	(interactive (list 'interactive))
	(cl-case command
					 (interactive (company-begin-backend 'company-fuel-backend))
					 (prefix (and (or (eq major-mode 'factor-mode)
														(eq major-mode 'fuel-mode))
												(company-grab-symbol)))
					 (candidates (fuel/complete-at-point-function))))

(add-hook 'factor-mode-hook (lambda ()
															(add-to-list 'company-backends 
																					 'company-fuel-backend)))

(add-hook 'factor-mode-hook (lambda ()
															(add-to-list 'completion-at-point-functions
																					 'fuel/complete-at-point-function)))
(provide 'factor-config)
