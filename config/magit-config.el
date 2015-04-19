(use-package magit
	:commands (magit-log magit-status)
	:ensure t
	:defer t
	:config (progn
						(setq magit-last-seen-setup-instructions "1.4.0")))

(provide 'magit-config)
