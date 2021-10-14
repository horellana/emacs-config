(require 'use-package)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-badger t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package mood-line
  :ensure t
  :disabled t
  :config (progn
      (mood-line-mode)))

(use-package tron-legacy-theme
  :ensure t
  :disabled t
  :config
  (load-theme 'tron-legacy t))

(use-package modus-operandi-theme
  :disabled t
  :ensure t
  :config (progn
	    (setq modus-themes-completions 'opitionated)
	    (setq modus-themes-fringes nil)
	    (setq modus-themes-lang-checkers '(straight-inderline intense))
	    (setq modus-themes-paren-match 'intense)
	    (setq modus-themes-region '(intense))
	    (load-theme 'modus-vivendi t)))

(use-package melancholy-theme
  :ensure t
  :disabled t
  :config (progn
	    (load-theme 'melancholy t)))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package tao-theme
  :ensure t
  :disabled t
  :config (progn
	    (load-theme 'tao-yin)))


(use-package smart-mode-line
  :ensure t
  :disabled t
  :config (progn
	    (setq sml/theme 'respectful)
	    (sml/setup)))

(set-frame-font "UbuntuMono-12")
