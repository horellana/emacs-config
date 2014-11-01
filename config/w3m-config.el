(use-package w3m
	:commands w3m
	:config (progn
						(setq browse-url-generic-program "w3m")
						(setq browse-url-browser-function
									(lambda (url &rest ignore)
										(w3m url t nil)))))

(provide 'w3m-config)
