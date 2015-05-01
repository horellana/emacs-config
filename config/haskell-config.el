(use-package haskell-mode
  :ensure t
	:defer t
  :config (progn
						(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
						(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
						
						(define-key haskell-mode-map (kbd "C-x C-d") nil)
						(define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
						(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
						(define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
						(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
						(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
						(define-key haskell-mode-map (kbd "C-c M-.") nil)
						(define-key haskell-mode-map (kbd "C-c C-d") nil)

						(custom-set-variables
						 '(haskell-process-type 'cabal-repl)
						 '(haskell-notify-p t)
						 '(haskell-stylish-on-save nil)
						 '(haskell-tags-on-save nil)
						 '(haskell-process-suggest-remove-import-lines t)
						 '(haskell-process-auto-import-loaded-modules t)
						 '(haskell-process-log t)
						 '(haskell-process-reload-with-fbytecode nil)
						 '(haskell-process-use-presentation-mode t)
						 '(haskell-interactive-mode-include-file-name nil)
						 '(haskell-interactive-mode-eval-pretty nil)
						 '(shm-use-presentation-mode t)
						 '(shm-auto-insert-skeletons t)
						 '(shm-auto-insert-bangs t)
						 '(haskell-process-suggest-haskell-docs-imports t)
						 '(hindent-style "chris-done")
						 '(haskell-interactive-mode-eval-mode 'haskell-mode)
						 ;; '(haskell-process-path-ghci "ghci-ng")
						 '(haskell-process-args-ghci '("-ferror-spans"))
						 '(haskell-process-generate-tags nil)
						 '(haskell-complete-module-preferred
							 '("Data.ByteString"
								 "Data.ByteString.Lazy"
								 "Data.Conduit"
								 "Data.Function"
								 "Data.List"
								 "Data.Map"
								 "Data.Maybe"
								 "Data.Monoid"
								 "Data.Ord")))))

(use-package flycheck
  :ensure t
	:defer t
  :config (progn
						(add-hook 'haskell-mode-hook 'flycheck-mode)))

(use-package flycheck-haskell
  :ensure t
	:defer t
  :config (progn
						(add-hook 'flycheck-mode 'flycheck-haskell-setup)))

(use-package company-ghci
	:ensure t
	:defer t
	:config (progn
						(add-hook 'haskell-mode-hook 'company-mode)
						(add-hook 'haskell-interactive-mode-hook 'company-mode)
						(add-to-list 'company-backends 'company-ghci)))

(use-package ac-haskell-process
  :ensure t
  :disabled t
	:defer t
  :config (progn
						(add-hook 'interactive-haskell-mode 'ac-haskell-process-setup)
						(add-hook 'interactive-haskell-mode-hook 'auto-complete-mode)
						(add-hook 'haskell-mode-hook 'ac-haskell-process-setup)))

(provide 'haskell-config)
