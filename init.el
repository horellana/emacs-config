;; -*- lexical-binding: t -*-

(setq gc-cons-threshold 50000000000)

(add-hook 'after-init-hook '(lambda () (setq gc-cons-threshold 800000)))

(eval-when-compile
  (require 'cl))

(require 'package)

(progn
  (add-to-list 'package-archives
	       '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.org/packages/") t)
  ;; (add-to-list 'package-archives
  ;; 	       '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (package-initialize)

  (when (not (package-installed-p 'req-package))
    (package-refresh-contents)
    (package-install 'req-package)))

(defun async-gtags-update ()
  (call-process "global" nil 0 nil "-u"))

(require 'req-package)

(req-package erc
  :defer t
  :commands (erc))

(req-package company
  :defer 1
  :config (progn
	    (setq company-minimum-prefix-length 3)
	    (setq company-show-numbers t)
	    (global-company-mode)))

(req-package company-quickhelp
  :disabled t
  :require (company)
  :config (add-hook 'company-mode-hook 'company-quickhelp-mode))

(req-package flycheck
  :defer t
  :config (progn
	    (custom-set-variables
	     '(flycheck-display-errors-function 
	       #'flycheck-pos-tip-error-messages))
	    (global-flycheck-mode)))

(req-package flycheck-pos-tip
  :require (flycheck))

(req-package smartparens
  :defer 1
  :config (progn
	    (sp-local-pair '(emacs-lisp-mode
			     lisp-mode
			     slime-repl-mode)
			   "`" nil :actions nil)
	    (sp-local-pair '(emacs-lisp-mode
			     lisp-mode
			     slime-repl-mode)
			   "'" nil :actions nil)  
	    (smartparens-global-strict-mode)))


(req-package slime-company
  :defer 1
  :require (company))

(req-package slime
  :require (slime-company)
  :init (progn
	  ;; (setq inferior-lisp-program "/home/juiko/git/sbcl/run-sbcl.sh")
	  (setq inferior-lisp-program "sbcl")
	  (setq slime-contrib '(slime-fancy
				slime-company))
	  (setq slime-sbcl-manual-root "/usr/local/share/info/sbcl.info")
	  (add-hook 'lisp-mode-hook
		    (lambda ()
		      (unless (slime-connected-p)
			(save-excursion (slime))))))
  :config (progn
	    (slime-setup '(slime-fancy slime-company))))

(req-package evil
  :config (progn
	    (define-key evil-visual-state-map (kbd "TAB") 'indent-region)
	    (define-key evil-normal-state-map (kbd "C-TAB") 'indent-whole-buffer)
	    (define-key evil-normal-state-map [return]
	      (lambda ()
		(interactive)
		(save-excursion
		  (newline))))
	    (setq evil-move-cursor-back nil)
	    
	    (cl-loop for mode in '(haskell-interactive-mode
				   haskell-presentation-mode
				   haskell-error-mode
				   sql-interactive-mode
				   inferior-emacs-lisp-mode
				   erc-mode
				   parparadox-menu-mode
				   comint-mode
				   eshell-mode
				   slime-repl-mode
				   slime-macroexpansion-minor-mode-hook
				   geiser-repl-mode
				   cider-repl-mode
				   inferior-python-mode)
		     do (evil-set-initial-state mode 'emacs))

	    
	    (evil-mode)
	    ))

(req-package evil-smartparens
  :require (evil smartparens)
  :config (progn
	    (add-hook 'smartparens-strict-mode-hook 'evil-smartparens-mode)))

(req-package evil-commentary
  :require (evil)
  :config (progn
	    (evil-commentary-default-setup)
	    (evil-commentary-mode)
	    ))

(req-package evil-god-state
  :require (evil god-mode)
  :config (progn
	    (bind-key "ESC" 'evil-normal-state evil-god-state-map)))

(req-package evil-leader
  :require (evil)
  :config (progn
	    (setq evil-leader/leader ",") 
	    (evil-leader/set-key
	      "f" 'find-file
	      "b" 'switch-to-buffer
	      "g" 'execute-extended-command
	      "k" 'kill-buffer
	      "," 'evil-execute-in-emacs-state
	      "p" 'helm-projectile
	      ";" 'comment-dwim
	      "e" 'eval-last-sexp
	      "w" 'save-buffer
	      "." 'ggtags-find-tag-dwim
	      "hs" 'helm-swoop
	      "ha" 'helm-ag
	      "hi" 'helm-semantic-or-imenu
	      "mgc" 'magit-checkout 
	      "mgl" 'magit-log
	      "mgs" 'magit-status)
	    
	    (evil-leader/set-key-for-mode 'haskell-mode "h" 'haskell-hoogle)
	    (evil-leader/set-key-for-mode 'emacs-lisp-mode "ma" 'pp-macroexpand-last-sexp)
	    (evil-leader/set-key-for-mode 'lisp-interaction-mode "ma" 'pp-macroexpand-last-sexp)

	    (evil-leader/set-key-for-mode 'lisp-mode "cl" 'slime-load-file)
	    (evil-leader/set-key-for-mode 'lisp-mode "e" 'slime-eval-last-expression)
	    (evil-leader/set-key-for-mode 'lisp-mode "me" 'slime-macroexpand-1)
	    (evil-leader/set-key-for-mode 'lisp-mode "ma" 'slime-macroexpand-all)
	    (evil-leader/set-key-for-mode 'lisp-mode "sds" 'slime-disassemble-symbol)
	    (evil-leader/set-key-for-mode 'lisp-mode "sdd" 'slime-disassemble-definition)
	    (evil-leader/set-key-for-mode 'cider-mode "e" 'cider-eval-last-sexp)
	    (evil-leader/set-key-for-mode 'projectile-mode (kbd "p")'helm-projectile)
	    (global-evil-leader-mode)))

(req-package evil-lisp-state
  :require (evil)
  :config (progn
	    (define-key evil-normal-state-map (kbd "L") 'evil-lisp-state)))

(defun juiko/look-config ()
  (blink-cursor-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode 1)
  (global-hl-line-mode 1)
  (show-paren-mode)
  (add-to-list 'default-frame-alist '(font . "Droid Sans Mono-9"))
  (add-to-list 'default-frame-alist '(cursor-color . "Gray")))

(req-package leuven-theme
  :config (progn
	    (add-hook 'after-init-hook
		      (lambda ()
			(load-theme 'leuven t)
			(set-face-attribute 'fringe
					    nil
					    :background "2e3436"
					    :foreground "2e3436")
			(juiko/look-config)))))

(req-package railscasts-theme
  :disabled t
  :config (progn
	    (load-theme 'railscasts t)
	    (juiko/look-config)))

(req-package projectile
  :config (progn
	    (add-hook 'after-init-hook 'projectile-global-mode)))

(req-package helm-config
  :config (progn

	    (setq helm-split-window-in-side-p t)

	    (add-to-list 'display-buffer-alist
			 '("\\`\\*helm.*\\*\\'"
			   (display-buffer-in-side-window)
			   (inhibit-same-window . t)
			   (window-height . 0.4)))

	    (setq helm-swoop-split-with-multiple-windows nil
		  helm-swoop-split-direction 'split-window-vertically
		  helm-swoop-split-window-function 'helm-default-display-buffer)

	    ;; (require 'helm-config)
	    
	    ;; (add-to-list 'helm-sources-list 
	    ;; 		 (helm-build-dummy-source "Create file"
	    ;; 		   :action 'find-file)
	    ;; 		 t)
	    (helm-mode)))

(req-package helm-projectile
  :commands (helm-mode)
  :require (helm projectile)
  :config (progn
	    (helm-projectile-on)))

(req-package helm-swoop
  :commands (helm-swoop)
  :require (helm))

(req-package helm-grep-ack
  :require (helm)
  :commands (helm-ack))

(req-package magit
  :defer 1
  :init (progn
	  (setq magit-last-seen-setup-instructions "1.4.0")))

(req-package haskell-mode
  :config (progn
	    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
	    (add-hook 'haskell-mode-hook 'haskell-doc-mode)
	    (add-hook 'haskell-mode-hook 'haskell-indent-mode)
	    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
	    (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
	    (add-hook 'haskell-mode-hook (lambda ()
					   (electric-indent-local-mode -1)))
	    
	    (setq haskell-process-type 'stack-ghci)
	    (setq haskell-process-path-ghci "stack")
	    (setq haskell-process-args-ghci ("ghci"))

	    (setq haskell-process-suggest-remove-import-lines t)
	    (setq haskell-process-auto-import-loaded-modules t)
	    (setq haskell-process-log t)))


(req-package hindent
  :require (haskell-mode)
  :config (progn
	    (setq hindent-style "chris-done")
	    (evil-define-key 'evil-visual-state hindent-mode-map "TAB"
	      'hindent-reformat-region)
	    ;; (bind-key "TAB" 'hindent-reformat-region hindent-mode-map)
	    (add-hook 'haskell-mode-hook 'hindent-mode)))

(req-package flycheck-haskell
  :require (flycheck haskell-mode)
  :defer t
  :config (progn
	    (add-hook 'haskell-mode-hook 'flycheck-mode)
	    (add-hook 'flycheck-mode-hook 'flycheck-haskell-configure)))

(req-package company-ghci
  :require (company haskell-mode)
  :config (progn
	    (push 'company-ghci company-backends)))

(req-package anaconda-mode
  :defer 1
  :config (progn
	    (add-hook 'python-mode-hook 'eldoc-mode)
	    (add-hook 'python-mode-hook 'anaconda-mode)))

(req-package company-anaconda
  :require (company anaconda-mode)
  :config (progn
	    (push 'company-anaconda company-backends)))

(req-package pyvenv
  :defer 1
  :config (progn
	    (add-hook 'python-mode-hook 'pyvenv-mode)))

(req-package yasnippet
  :defer 1)

(req-package irony
  :defer t
  :config (progn
	    (add-hook 'c-mode-hook 'irony-mode)
	    (add-hook 'c++-mode-hook 'irony-mode)
	    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(req-package company-irony
  :require (company irony)
  :commands (irony-mode)
  :config (progn
	    (push 'company-irony company-backends)))

(req-package flycheck-irony
  :require (flycheck irony)
  :config (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

(req-package irony-eldoc
  :require (irony)
  :config (progn
	    (add-hook 'irony-mode-hook 'eldoc-mode)))

(req-package vagrant)

(req-package vagrant-tramp
  :config (progn
	    (eval-after-load 'tramp
	      '(vagrant-tramp-enable))))

(req-package web-mode
  :config (progn
	    (add-hook 'web-mode-hook
		      (lambda ()
			(setq web-mode-code-indent-offset 2)
			(setq web-mode-markup-indent-offset 2)
			(setq web-mode-css-indent-offset 2)))
	    (add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
	    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
	    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
	    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
	    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
	    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
	    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
	    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))))

(req-package php-mode
  :defer t
  :config (progn
	    (require 'php-ext)
	    (add-hook 'after-save-hook 'async-gtags-update)
	    (setq php-template-compatibility nil)))

(req-package-finish)

(defun endless/upgrade ()
  "Upgrade all packages, no questions asked."
  (interactive)
  (save-window-excursion
    (list-packages)
    (package-menu-mark-upgrades)
    (package-menu-execute 'no-query)))

(eldoc-mode t)
(setq inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)

(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.emacs.d/saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)


(global-set-key (kbd "C-%") 'iedit-mode)
(global-set-key (kbd "M--") 'hippie-expand)
(global-set-key (kbd "M-g M-g") 
		'(lambda ()
		   (interactive)
		   (unwind-protect
		       (progn
			 (linum-mode t)
			 (call-interactively 'goto-line))
		     (linum-mode -1))))

(add-hook 'after-save-hook
	  (lambda ()
	    (let ((init-file (expand-file-name "~/.emacs.d/init.el")))
	      (when (equal (buffer-file-name) init-file)
		(byte-compile-file init-file)))))

(setq browse-url-browser-function 'browse-url-chromium)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(package-selected-packages
   (quote
    (helm-gtags php-mode web-mode railscasts-theme hindent company-ghc ghc yasnippet wgrep-helm wgrep-ag slime-company shm req-package pyvenv magit leuven-theme irony-eldoc helm-swoop helm-projectile helm-ag flycheck-pos-tip flycheck-irony flycheck-haskell evil-smartparens evil-lisp-state evil-god-state evil-commentary company-ycmd company-quickhelp company-irony company-ghci company-anaconda benchmark-init))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
