(eval-after-load "org"
  '(progn
     (setq org-agenda-files (list "~/emacs-org/todos.org"))

     (define-key global-map "\C-Ca" 'org-agenda)
     (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))))
