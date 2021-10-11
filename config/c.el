(require 'projectile)

(use-package platformio-mode
  :ensure t
  :config (progn
	    (add-hook 'c++-mode-hook (lambda ()
				       (platformio-conditionally-enable)))))


(defun horellana/c-mode-update-tags ()
  (progn
    (when (and (eq major-mode 'c++-mode)
	       (projectile-project-root))

      (let* ((default-directory (projectile-project-root)))
	(message "updating tags")
	(message "tags updated")))))

(defun horellana/platformio-update-tags ()
  )

(with-eval-after-load "lsp-mode"
  (add-to-list 'lsp-enabled-clients 'clangd)
  (add-to-list 'lsp-enabled-clients 'ccls))

(add-hook 'after-save-hook 'horellana/c-mode-update-tags)
(add-hook 'after-save-hook 'horellana/platformio-update-tags)
