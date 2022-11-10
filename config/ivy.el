(use-package ivy
  :disabled t
  :ensure t
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("<f1> f" . counsel-describe-function)
	 ("<f1> v" . counsel-describe-variable)
	 ("<f1> o" . counsel-describe-symbol)
	 ("<f1> l" . counsel-find-library)
	 ("<f2> i" . counsel-info-lookup-symbol)
	 ("<f2> u" . counsel-unicode-char)
	 ("C-c g" . counsel-git)
	 ("C-c j" . counsel-git-grep)
	 ("C-c k" . counsel-rg)
	 ("C-x l" . counsel-locate)
	 ("C-x p p" . project-switch-project)
	 ("C-x b" . ivy-switch-buffer)
	 ("C-s" . swiper))
	 
  :config (progn
	    (setq ivy-use-virtual-buffers t)
	    (setq ivy-display-style 'fancy)
	    (setq ivy-height 10)
	    (setq enable-recursive-minibuffers t)
	    (ivy-mode 1)
	    (counsel-mode 1)))

(use-package counsel
  :after (ivy)
  :ensure t)

(use-package counsel-projectile
  :disabled t
  :commands (counsel-projectile counsel-projectile-ag counsel-projectile-grep)
  :bind (("C-c p p" . counsel-projectile-switch-project))
  :after (ivy counsel projectile)
  :config (progn
	    (counsel-projectile-mode)))

(use-package ivy-rich
  :ensure t
  :after (ivy)
  :config (progn
	    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
	    (ivy-rich-mode 1)))

(use-package all-the-icons-ivy-rich
  :ensure t
  :after (ivy ivy-rich)
  :init (all-the-icons-ivy-rich-mode 1))
