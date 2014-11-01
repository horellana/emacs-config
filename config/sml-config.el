(use-package smart-mode-line
	:disabled t
	:config (progn
						(setq sml/hidden-modes nil)
						(sml/toggle-shorten-modes)

						(setq sml/shorten-directory t)
						(setq sml/shorten-modes t)


						(add-to-list 'sml/replacer-regexp-list
												 '("^~/Dropbox/Projects/In-Development/" ":ProjDev:"))

						(add-to-list 'sml/replacer-regexp-list '
												 ("^~/Documents/Work/" ":Work:"))

						;; Added in the right order, they even work sequentially:
						(add-to-list 'sml/replacer-regexp-list
												 '("^:DB:Documents" ":DDocs:"))

						(add-to-list 'sml/replacer-regexp-list
												 '("^~/Dropbox/" ":DB:"))

						(add-to-list 'sml/replacer-regexp-list
												 '("^:Git:\\(.*\\)/src/main/java/" ":G/\\1/SMJ:"))

						(add-to-list 'sml/replacer-regexp-list
												 '("^~/Git-Projects/" ":Git:"))

						(sml/setup)))

(use-package smart-mode-line-powerline-theme
	:disabled t)

(provide 'sml-config)
