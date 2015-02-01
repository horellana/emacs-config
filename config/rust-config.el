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

(progn
	(add-to-list 'load-path "/home/juiko/git/racer/editors")
	(setenv "RUST_SRC_PATH" "/home/juiko/git/rust/src/")
	(use-package racer
		:config (setq racer-rust-src-path "/home/juiko/git/rust/src/"
									racer-cmd "/home/juiko/git/racer/target/release/racer")))

(provide 'rust-config)
