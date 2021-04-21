(require 'use-package)

(use-package tron-legacy-theme
  :ensure t
  :config (progn
      (load-theme 'tron-legacy t)))

(req-package mood-line
  :ensure t
  :config (progn
      (mood-line-mode)))
