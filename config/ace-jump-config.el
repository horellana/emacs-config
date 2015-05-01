(use-package ace-jump-mode
  :defer t
  :ensure t
  :config (progn
            (global-set-key (kbd "M-c") 'ace-jump-char-mode)
            (global-set-key (kbd "M-q") 'ace-jump-word-mode)
            (global-set-key (kbd "M-l") 'ace-jump-line-mode)))

(provide 'ace-jump-config)
