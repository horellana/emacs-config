(use-package rust-mode
	:ensure t)

(use-package company
	:ensure t
	:config (progn
						(add-hook 'rust-mode-hook 'company-mode)))

(use-package flycheck
	:ensure t
	:config (progn
						(add-hook 'rust-mode-hook 'flycheck-mode)))

(use-package aggressive-indent
	:ensure t
	:config (progn
						(add-hook 'rust-mode-hook 'aggressive-indent-mode)))

(use-package emacs-rust-helpers)

(eval-after-load "rust-mode"
	(progn (add-to-list 'load-path "/home/juiko/git/racer/editors")
				 (use-package racer
					 :config (progn
										 (setq racer-rust-src-path "/home/juiko/git/rust/src/"
													 racer-cmd "/home/juiko/git/racer/target/release/racer")))))

(eval-after-load "flycheck"
	(progn
		(use-package flycheck-rust
			:ensure t
			:defer t
			:config (progn
								(add-hook 'flycheck-mode-hook 'flycheck-rust-setup)))))


(provide 'rust-config)
