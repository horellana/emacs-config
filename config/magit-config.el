(use-package magit
	:ensure t
	:defer t)

(eval-after-load "magit"
	(setq magit-last-seen-setup-instructions "1.4.0"))

(provide 'magit-config)
