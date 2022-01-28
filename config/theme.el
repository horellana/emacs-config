(define-advice load-theme (:before (&rest _args) theme-dont-propagate)
  "Discard all themes before loading new."
  (mapc #'disable-theme custom-enabled-themes))

(require 'use-package)

(use-package doom-themes
  :ensure t
  :disabled t
  :config (progn
	 (setq doom-themes-enable-bold t
	       doom-themes-enable-italic t)

	 (load-theme 'doom-dracula t)

	 (doom-themes-visual-bell-config)
	 (doom-themes-neotree-config)
	 (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
	 (doom-themes-treemacs-config)
	 (doom-themes-org-config)))


(use-package tao-theme
  :ensure t
  :config (progn
	    (load-theme 'tao-yin t)))

(set-frame-font "JetBrains Mono-11")
