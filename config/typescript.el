(use-package typescript-mode
  :disabled t
  :ensure t
  :config (progn
            (setq-default typescript-indent-level 2)
            (eval-after-load "lsp-mode"
              '(progn
                 (message "Loading lsp for typescript")
                 (setq lsp-clients-angular-language-server-command
                       '("node"
                         "/usr/lib/node_modules/@angular/language-server"
                         "--ngProbeLocations"
                         "/usr/lib/node_modules"
                         "--tsProbeLocations"
                         "/usr/lib/node_modules"
                         "--stdio"))
                 (add-hook 'typescript-mode-hook 'lsp)
                 ))))
