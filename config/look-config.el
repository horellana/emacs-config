(require 'cl)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(add-to-list 'default-frame-alist '(font . "Terminus-9"))

(use-package color-theme
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

(provide 'look-config)
