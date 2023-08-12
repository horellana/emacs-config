(use-package lsp-mode
  :disabled t
  :ensure t
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (rust-mode . lsp-deferred))
         ;; (js-mode . lsp-deferred)
         ;; (js-jsx-mode . lsp-deferred)
         ;; (typescript-mode . lsp-deferred))

  :config (progn
            (setq lsp-ui-doc-enable t)
            (setq lsp-idle-delay 0.1)
            (setq lsp-restart 'auto-restart)

            (add-to-list 'lsp-enabled-clients 'ts-ls)
            (add-to-list 'lsp-enabled-clients 'clangd)
            (add-to-list 'lsp-enabled-clients 'rust-analyzer)

            (add-hook 'lsp-mode-hook
                      (lambda ()
                        (setq-local company-backends
                                    '(company-capf company-dabbrev-code company-dabbrev))))

            (with-eval-after-load "yasnippet"
              (add-hook 'lsp-mode-hook 'yas-minor-mode))))

(use-package lsp-ui
  :disabled t
  :after (lsp-mode)
  :commands (lsp lsp-deferred)
  :ensure t)

(use-package lsp-ivy
  :disabled t
  :after (lsp-mode ivy)
  :commands (lsp lsp-deferred)
  :ensure t)
