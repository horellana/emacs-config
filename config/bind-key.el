(use-package bind-key
  :ensure t
  :config (with-eval-after-load "bind-key"
	    '(progn
	       (bind-key (kbd "M--") 'hippie-expand)
	       (bind-key (kbd "M-g M-g")
			 (lambda ()
			   (interactive)
			   (unwind-protect
			       (progn
				 (linum-mode t)
				 (call-interactively 'goto-line))
			     (linum-mode -1)))))))
