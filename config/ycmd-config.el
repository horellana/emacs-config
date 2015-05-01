(use-package ycmd
	:ensure t
	:disabled t
	:config (progn
						(set-variable 'ycmd-server-command 
													'("python2" "/home/juiko/git/ycmd/ycmd"))
						(ycmd-setup)))

(use-package company-ycmd
	:ensure t
	:disabled t
	:config (progn
						(add-hook 'company-mode-hook 'company-ycmd-setup)))

(provide 'ycmd-config)
