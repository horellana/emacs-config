(use-package tramp
  :defer t
  :config (progn
            (setq tramp-default-method "ssh")))

(provide 'tramp-config)
