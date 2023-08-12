(use-package notmuch
  :ensure t
  :config (progn
	    (setq notmuch-multipart/alternative-discouraged '("text/plain"))
	    (setq mm-text-html-renderer 'links)))
