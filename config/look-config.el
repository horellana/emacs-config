(require 'cl)


(defun my-tango-theme ()
	(load-theme 'tango-dark t)
	(set-face-attribute 'fringe
											nil
											:background "#2e3436"
											:foreground "#2e3436")
	(set-face-foreground 'mode-line "white")
	(set-face-background 'mode-line "#2e3436"))

(defun black-theme ()
  (load-theme 'manoj-dark t)
  (set-face-attribute 'fringe
											nil
											:background "#000000"
											:foreground "#000000"))


(defun juiko/tomorrow-color-theme ()
  (load 
   "/home/juiko/git/tomorrow-night-paradise-theme.el/tomorrow-night-paradise-theme.el")
  (set-face-attribute 'fringe
											nil
											:background "#000000"
											:foreground "#000000"))

(use-package solarized-theme
	:disabled t
  :ensure t
  :config (progn
						(load-theme 'solarized-dark t)
						(set-face-attribute 'fringe
																nil
																:background "#002b36"
																:foreground "#002b36")))

(use-package leuven-theme
  :ensure t
  :config (progn
						(load-theme 'leuven t)
						(set-face-attribute 'fringe
																nil
																:background "2e3436"
																:foreground "2e3436")))

(use-package color-theme
  :disabled t
  :ensure t
  :config (progn
						(load 
						 "/home/juiko/git/color-theme-mac-classic/color-theme-mac-classic.el")
						(color-theme-mac-classic)))

(use-package powerline
  :disabled t
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
																 2e3436space-mode
																 flycheck-mode)
									 do (ignore-errors (diminish mode))))

(progn
	(blink-cursor-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  (add-to-list 'default-frame-alist '(font . "Droid Sans Mono-9"))
  (add-to-list 'default-frame-alist '(cursor-color . "Gray"))

  (set-display-table-slot standard-display-table 
													'vertical-border 
													(make-glyph-code 8203)))


(provide 'look-config)
