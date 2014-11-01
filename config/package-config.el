(defvar package-sources '(("marmalade" . "http://marmalade-repo.org/packages/")
													("melpa" . "http://melpa.milkbox.net/packages/"))) 

(dolist (source package-sources)
  (add-to-list 'package-archives source))

(provide 'package-config)
