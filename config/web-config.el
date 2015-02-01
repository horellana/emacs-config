(use-package web-mode
	:ensure t
	:commands (web-mode)
	:config (progn
						(defalias 'html-mode 'web-mode)
						(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
						(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
						(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
						(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
						(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
						(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
						(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
						(add-to-list 'auto-mode-alist '("\\.html" . web-mode))

						(setq web-mode-ac-sources-alist
									'(("css" . (ac-source-css-property))
										("html" . (ac-source-words-in-buffer ac-source-abbrev))))
						(add-hook 'web-mode-hook 'auto-complete-mode)))


(use-package js2-mode
	:ensure t
	:commands (js2-mode javascript-mode)
	:config (progn
						(use-package ac-js2
							:ensure t
							:config (progn
												(add-hook 'js2-mode-hook 'ac-js2-mode)
												(defalias 'javascript-mode 'js2-mode)
												(add-hook 'js2-mode-hook 'rainbow-mode)))))														
(use-package rainbow-mode
	:ensure t
	:config (progn
						(add-hook 'web-mode-hook 'rainbow-mode)
						(add-hook 'html-mode-hook 'rainbow-mode)
						(add-hook 'css-mode-hook 'rainbow-mode)))

(use-package aggressive-indent
	:ensure t
	:config (progn
						(add-hook 'web-mode-hook 'aggressive-indent-mode)))

(provide 'web-config)
