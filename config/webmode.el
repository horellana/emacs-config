(use-package web-mode
  :ensure t
  :mode ("\\.js\\'" "\\.jsx\\'" "\\.ts\\'" "\\.tsx\\'")
  :config (progn
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

               (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
               (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

               (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))))
