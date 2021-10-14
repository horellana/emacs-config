(use-package evil-org
  :requires (evil)
  :config (progn
	    (add-hook 'org-mode-hook 'evil-org-mode)
	    (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
	    (require 'evil-org-agenda)
	    (evil-org-agenda-set-keys)))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "/home/hector/Documents/org-files/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  ;; (require 'org-roam-protocol)
  )

(eval-after-load "org"
  '(progn
     (setq org-agenda-files (list "~/emacs-org/todos.org"))

     (define-key global-map "\C-Ca" 'org-agenda)
     (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))))
