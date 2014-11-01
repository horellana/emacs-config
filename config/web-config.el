(use-package web-mode
	:ensure t
	:commands (web-mode)
	:config (progn
						(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
						(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
						(add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
						(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
						(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
						(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
						(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

						;; (use-package ac-html
						;; 							:config (progn
						;; 												(add-to-list ac-sources ac-html-completion)))
						
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

(provide 'web-config)
