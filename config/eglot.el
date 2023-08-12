(use-package eglot
  :ensure t
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (sh-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (python-mode . eglot-ensure))
  :config (progn
            (setq eglot-sync-connect 0)
            (add-to-list 'eglot-server-programs
                         '(web-mode . ("typescript-language-server" "--stdio")))))
