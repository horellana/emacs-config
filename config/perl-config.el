(defalias 'perl-mode 'cperl-mode)

(use-package cperl-mode
  :commands cperl-mode
  :config (progn
	    (add-to-list 'auto-mode-alist '("\\.t\\'" . cperl-mode))
	    (add-hook 'cperl-mode-hook
		      (lambda ()
			(cperl-set-style "K&R")
			(setq 
			 cperl-indent-level 2)))))

(use-package auto-complete
  :ensure t
  :commands (cperl-mode)
  :config (progn
	    (add-hook 'cperl-mode-hook
		      (lambda ()
			(loop for source in '(ac-source-words-in-buffer
					      ac-source-words-in-same-mode-buffers) 
			      do (add-to-list 'ac-sources source))))
	    

	    (use-package ac-etags
	      :ensure t
	      :config (progn
			(add-hook 'cperl-mode-hook
				  'ac-etags-ac-setup)
			(add-hook 'cperl-mode-hook
				  'ac-etags-setup)))

	    (add-hook 'cperl-mode-hook 'auto-complete-mode)))

(use-package yasnippet
  :ensure t
  :commands (cperl-mode)
  :config (progn
	    (add-hook 'cperl-mode-hook 'yas-minor-mode)))

(use-package flycheck
  :ensure t
  :commands (cperl-mode)
  :config (progn
	    (add-hook 'cperl-mode-hook 'flycheck-mode)))

(use-package perlp
  :disabled t
  :config (progn
	    (perlp/setup-perl-project-lib)))

(use-package aggressive-indent
  :config (progn
	    (add-hook 'cperl-mode-hook 'aggressive-indent-mode)))

(progn
  (require 'utilities)

  (defun juiko/perl-create-etags (project-root)
    (let ((files (mapcar (lambda (a) (concat project-root a))
			 (projectile-current-project-files))))
      (shell-command (format "xargs etags -l perl -o %s" 
			     (intersperce " " files))

		     "*shell-command*"
		     "*shell-commmand-error*")
      (message "")))

  (defun juiko/perl-etags-hook ()
    (when (and (projectile-project-root) (equal major-mode 'cperl-mode))
      (juiko/perl-create-etags (projectile-project-root)))))

;; (add-hook 'after-find-file 'juiko/perl-etags-hook)
;; (add-hook 'after-save-hook 'juiko/perl-etags-hook))

(provide 'perl-config)
