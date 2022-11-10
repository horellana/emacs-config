;; (require 'projectile)

(use-package platformio-mode
  :ensure t
  :config (progn
	    (add-hook 'c++-mode-hook (lambda ()
				       (platformio-conditionally-enable)))))
