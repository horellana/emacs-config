(req-package pipenv
  :ensure t
  :hook (python-mode . pipenv-mode)
  :config (progn
	    (defun pipenv-shell ()
	      (message "(pipenv-shell) STUB"))
	    (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended)
	    (add-hook 'python-mode-hook 'pipenv-activate)))

(eval-after-load "lsp"
  '(progn
     (add-hook 'python-mode-hook 'lsp)
     (add-hook 'python-mode-hook
	       (lambda ()
		 (setq-local company-backends '(company-capf))))))


(defun horellana/setup-project-pipenv ()
  (if venv-directory
	(set-pipenv-directory-dir-local)))

(defun horellana/get-pipenv-directory (project-root)
  (let ((default-directory project-root))
    (shell-command-to-string "printf %s \"$(pipenv --where)\"")))

(defun horellana/set-pipenv-directory-dir-local ()
  (when (projectile-project-root)
    (let* ((default-directory (projectile-project-root))
	   (pipenv-directory (horellana/get-pipenv-directory default-directory)))
      (add-dir-local-variable 'python-mode 'venv-directory pipenv-directory)
      pipenv-directory)))
