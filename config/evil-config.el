(defun evil-escape-works ()
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit))


(use-package evil
						 :ensure t
						 :config (progn
											 (define-key evil-visual-state-map (kbd "TAB") 'indent-region)
											 (define-key evil-normal-state-map (kbd "C-TAB") 'indent-whole-buffer)

											 (evil-escape-works)

											 (define-key evil-normal-state-map [return]
												 (lambda ()
													 (interactive)
													 (save-excursion
														(newline))))

											 (define-key evil-normal-state-map (kbd "-")
												 (lambda ()
													 (interactive)
													 (end-of-line)
													 (ac-complete-with-helm)))

											 (define-key evil-visual-state-map (kbd ";") 'comment-dwim)

											 (defadvice eval-last-sexp (around evil)
												 "Last sexp ends at point."
												 (when (evil-normal-state-p)
													 (save-excursion
														(unless (or (eobp) (eolp)) (forward-char))
														ad-do-it)))

											 (defun evil-ac-helm ()
												 (interactive)
												 (save-excursion
													(forward-char)
													(ac-complete-with-helm)))

											 (loop for mode in '(haskell-interactive-mode
																					 slime-repl-mode)
														 do (evil-set-initial-state mode 'emacs))
											 
											 (setf evil-move-cursor-back nil)
											 (evil-mode)))

(use-package god-mode
						 :ensure t
						 :config (progn
											 (use-package evil-god-state
																		:ensure t
																		:config (progn
																							(define-key evil-god-state-map [escape] 'evil-normal-state)
																							(global-set-key (kbd "C-x C-1") 'delete-other-windows)
																							(global-set-key (kbd "C-x C-2") 'split-window-below)
																							(global-set-key (kbd "C-x C-3") 'split-window-right)
																							(global-set-key (kbd "C-x C-0") 'delete-window)))))


(use-package evil-paredit
						 :disabled t
						 :ensure t
						 :config (progn
											 (add-hook 'lisp-mode-hook 'evil-paredit-mode)
											 (add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)))

(use-package evil-surround
						 :ensure t
						 :config (progn
											 (global-evil-surround-mode)
											 (global-subword-mode t)))

(use-package evil-lisp-state
						 :ensure t
						 :config (progn
											 (define-key evil-normal-state-map (kbd "L") 'evil-lisp-state)))

(use-package evil-leader
						 :ensure t
						 :config (progn
											 (setq evil-leader/leader ",") 
											 (evil-leader/set-key
												"f" 'find-file
												"b" 'switch-to-buffer
												"g" 'execute-extended-command
												"k" 'kill-buffer
												"," 'evil-execute-in-god-state
												"p" 'helm-projectile
												";" 'comment-dwim
												"e" 'eval-last-sexp
												"." 'ggtags-find-tag-dwim)
											 (global-evil-leader-mode)))

(use-package evil-matchit
						 :ensure t)

(use-package evil-nerd-commenter
						 :ensure t
						 :config (progn
											 (evilnc-default-hotkeys)))

(use-package evil-jumper
						 :ensure t)

(provide 'evil-config)
