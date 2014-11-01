(use-package factor-mode
  :commands (factor-mode fuel-mode)
  :config (progn
            (custom-set-variables '(factor-indent-level 2))
            (use-package auto-complete :config (progn
                                                 (add-hook 'factor-mode-hook
                                                           (lambda ()
                                                             (make-local-variable 'ac-sources)
                                                             (setq-local ac-sources '(ac-source-words-in-buffer
                                                                                      ac-source-words-in-same-mode-buffers))
                                                             (auto-complete-mode)))))

            (setq factor-root-dir "/home/juiko/git/factor/")
            (setq fuel-listener-factor-binary (concat factor-root-dir "factor"))
            (setq fuel-listener-factor-image (concat factor-root-dir "factor.image"))
            (add-hook 'factor-mode-hook (lambda () (fuel-mode)))
            (add-hook 'factor-mode-hook (lambda () (show-paren-mode)))))

(provide 'factor-config)
