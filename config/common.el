(progn
  (blink-cursor-mode -1)

  (when (not (eq system-type 'darwin))
    (menu-bar-mode -1))

  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode 1)
  ;; (global-hl-line-mode)
  (show-paren-mode 1)
  (eldoc-mode t)
  (setq ccls-executable "/usr/bin/ccls")
  (setq js-indent-level 2)
  (setf tab-width 2)
  (setf tramp-default-method "ssh")
  (setf indent-tabs-mode nil)
  (setf inhibit-startup-message t)
  (setf browse-url-generic-program "firefox")
  (setf browse-url-browser-function 'browse-url-firefox)
  (setf ring-bell-function 'ignore)
  (setf org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
  (setf *no-smartparens-list* '(haskell-mode))
  (setf emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
  (setq create-lockfiles nil)
  (setf backup-by-copying t      ; don't clobber symlinks
	backup-directory-alist '(("~/.emacs.d/saves"))    ; don't litter my fs tree
	delete-old-versions t
	kept-new-versions 6
	kept-old-versions 2
	version-control t)
  (add-hook 'prog-mode-hook 'hs-minor-mode)
  (add-hook 'prog-mode-hook 'outline-minor-mode)

  (add-hook 'after-save-hook 'whitespace-cleanup)

  (add-hook 'after-save-hook
	    (lambda ()
	      (let ((init-file (expand-file-name "~/.emacs.d/init.el")))
		(when (equal (buffer-file-name) init-file)
		  (byte-compile-file init-file)))))
  )

(set-face-attribute 'fringe nil
		    :foreground (face-foreground 'default)
		    :background (face-background 'default))

(fset 'yes-or-no-p 'y-or-n-p)
(set-frame-font "UbuntuMono-12")

(req-package no-littering
  :ensure t
  :defer t
  :config (progn
	    (setq auto-save-file-name-transforms
		  `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))))
