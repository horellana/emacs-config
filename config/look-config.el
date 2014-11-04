(require 'cl)


(use-package leuven-theme
						 :ensure t
						 :config (progn
											 (load-theme 'leuven t)
											 
											 (set-face-attribute 'fringe
																					 nil
																					 :background "white"
																					 :foreground "white")))

(use-package color-theme
						 :disabled t
						 :ensure t
						 :config (progn
											 (load "/home/juiko/git/color-theme-mac-classic/color-theme-mac-classic.el")
											 (color-theme-mac-classic)))

(use-package powerline
						 :ensure t
						 :config (use-package powerline-evil
																	:ensure t
																	:config (powerline-evil-vim-color-theme)))

(use-package diminish
						 :defer t
						 :ensure t
						 :config (cl-loop for mode in '(helm-mode
																						auto-complete-mode
																						undo-tree-mode
																						company-mode
																						eldoc-mode
																						paredit-mode
																						projectile-mode
																						company-mode
																						whitespace-mode
																						flycheck-mode)
															do (ignore-errors (diminish mode))))

(progn
	(menu-bar-mode -1)
	(tool-bar-mode -1)
	(scroll-bar-mode -1)

	(add-to-list 'default-frame-alist '(font . "Droid Sans Mono-10"))

	(set-display-table-slot standard-display-table 
													'vertical-border (make-glyph-code 8203))

	(set-cursor-color "gray"))

(provide 'look-config)
