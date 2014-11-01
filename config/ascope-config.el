(use-package ascope
             :disabled t
             :commands (ascope-pop-mark ascope-find-global-definition)
             :config (progn
                       (use-package cc-mode)
                       (define-key c-mode-base-map (kbd "C-.")
                         (lambda ()
                           (interactive)
                           (call-interactively 'ascope-find-global-definition)))

                       (define-key c-mode-base-map (kbd "C-*")
                         (lambda ()
                           (interactive)
                           (ascope-pop-mark)))))

(provide 'ascope-config)
