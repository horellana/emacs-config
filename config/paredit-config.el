(use-package paredit
	:commands paredit-mode
	:config (progn
						(define-key paredit-mode-map (kbd "C-M-h") 'paredit-backward-slurp-sexp)
						(define-key paredit-mode-map (kbd "C-M-l") 'paredit-forward-slurp-sexp)))


(provide 'paredit-config)
