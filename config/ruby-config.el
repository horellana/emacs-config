(use-package enh-ruby-mode
	:ensure t)

(use-package flycheck
	:ensure t
	:config (progn
						(add-hook 'enh-ruby-mode-hook 'flycheck-mode)))

(use-package inf-ruby
	:ensure t
	:config (progn
						(add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)))

(use-package auto-complete
	:ensure t
	:config (progn
						(add-hook 'enh-ruby-mode-hook 'auto-complete-mode)))

(use-package robe
	:ensure t
	:config (progn
						(add-hook 'enh-ruby-mode-hook 'robe-mode)
						(add-hook 'robe-mode-hook 'ac-robe-setup)))

(use-package ruby-end
	:ensure t
	:config (progn
						(add-hook 'enh-ruby-mode-hook 'ruby-end-mode)))

(provide 'ruby-config)
