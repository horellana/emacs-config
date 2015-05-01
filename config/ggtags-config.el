(use-package ggtags
	:ensure t
	:config (progn
						(setq
						 helm-gtags-ignore-case t
						 helm-gtags-auto-update t
						 helm-gtags-use-input-at-cursor t
						 helm-gtags-pulse-at-cursor t
						 helm-gtags-prefix-key "\C-cg"
						 helm-gtags-suggested-key-mapping t)))

(use-package helm-gtags
	:ensure t)


(provide 'ggtags-config)
