(req-package ivy
  :ensure t
  ;; :commands (counsel-M-x counsel-find-file counsel-describe-function ivy-switch-buffer)
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-h f" . counsel-describe-function)
	 ("C-x b" . ivy-switch-buffer))
  :config (progn
	    (ivy-mode 1)
	    (counsel-mode 1))
  )

(req-package counsel-projectile
  :ensure t
  :commands (counsel-projectile counsel-projectile-ag counsel-projectile-grep)
  :bind (("C-c p p" . counsel-projectile-switch-project))
  :config (progn
	    (counsel-projectile-mode)))

(eval-after-load "evil-lisp-state"
  '(progn
     (evil-leader/set-key
       "hP"  'counsel-projectile
       "hpa" 'counsel-projectile-ag
       "hpg" 'counsel-projectile-grep
       "g" 'counsel-M-x
       "f" 'counsel-find-file
       "b" 'ivy-switch-buffer
       "hs" 'swiper)))
