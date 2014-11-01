(use-package ido
  :disabled t
  :config (progn
            (use-package ido-at-point)
            (use-package ido-vertical-mode)
            (use-package ido-select-window)
            (use-package ido-yes-or-no)
            (use-package ido-hacks)
            (use-package ido-ubiquitous)

            (defun ido-recentf-open ()
              "Use `ido-completing-read' to \\[find-file] a recent file"
              (interactive)
              (if (find-file (ido-completing-read "Find recent file: " recentf-list))
                  (message "Opening file...")
                (message "Aborting")))

            (defun ido-dired ()
              "Use `ido-completing-read' to \\[find-file] a recent file"
              (interactive)
              (if (dired (ido-list-directory))
                  (message "Opening directory...")
                (message "Aborting")))

            (ido-mode)
            (ido-everywhere)
            (ido-vertical-mode)
            (ido-at-point-mode)
            (ido-ubiquitous-mode)

            ;; (global-set-key (kbd "C-x o") 'ido-select-window)
            (global-set-key (kbd "C-x p k") 'kill-buffer-match-regexp)
            (global-set-key (kbd "C-x C-b") 'ido-switch-buffer)))

(provide 'ido-config)
