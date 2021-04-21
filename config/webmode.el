(req-package web-mode
  :ensure t
  :hook (html-mode . web-mode)
  :mode (("\\.blade\\.php\\'" . web-mode))
  :config (eval-after-load "web-mode"
	    '(progn
	       (add-hook 'web-mode-hook #'turn-off-smartparens-mode)

	       (add-hook 'web-mode-hook
			 (lambda ()
			   (setf web-mode-enable-auto-pairing t)
			   (setf web-mode-enable-css-colorization t)
			   (setf web-mode-enable-block-face t)
			   (setf web-mode-enable-heredoc-fontification t)
			   (setf web-mode-enable-current-element-highlight nil)
			   (setf web-mode-enable-current-column-highlight nil)
			   (setf web-mode-code-indent-offset 2)
			   (setf web-mode-markup-indent-offset 2)
			   (setf web-mode-css-indent-offset 2)))

	       (cl-loop
		for extension in '("\\.blade\\.php\\'"
				   "\\.ejs\\'"
				   "\\.phtml\\'"
				   "\\.tpl\\.php\\'"
				   "\\.[agj]sp\\'"
				   "\\.as[cp]x\\'"
				   "\\.erb\\'"
				   "\\.mustache\\'"
				   "\\.djhtml\\'"
				   "\\.html\\'"
				   "\\.html\\.erb\\'"
				   "\\html\\.twig\\'"
				   "\\html\\.jinja\\'"
				   "\\pdf\\.twig\\'"
				   "\\.jsx\\'"
				   "\\.blade\\.php\\'")
		do (add-to-list 'auto-mode-alist `(,extension . web-mode))))))
