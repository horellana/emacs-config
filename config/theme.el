(define-advice load-theme (:before (&rest _args) theme-dont-propagate)
  "Discard all themes before loading new."
  (mapc #'disable-theme custom-enabled-themes))

(require 'use-package)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config (progn
            (setq doom-modeline-buffer-file-name-style 'buffer-name)
            (doom-modeline-set-modeline 'main)
            (setq doom-modeline-display-default-persp-name t)
            (setq doom-modeline-enable-wordcount t)))


(use-package immaterial-theme
  :ensure t
  :disabled t
  :config (progn
            (load-theme 'immaterial-dark t)))

(use-package ef-themes
  :ensure t
  :config (progn
            (load-theme 'ef-dark t)))

;; (load-theme 'wombat t)

(set-frame-font "JetBrains Mono-11")

(setq line-number-mode t)
(setq column-number-mode t)
(size-indication-mode 1)

(set-face-attribute 'fringe nil
                    :foreground (face-foreground 'default)
                    :background (face-background 'default))
