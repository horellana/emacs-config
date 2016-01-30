;; -*- lexical-binding: t -*-

(setf lexical-binding t)

(let ((original-gc-cons-threshold gc-cons-threshold))
  (setq gc-cons-threshold 50000000000)
  (add-hook 'after-init-hook
	    (lambda ()
	      (setq gc-cons-threshold original-gc-cons-threshold))))

(eval-when-compile
  (require 'cl))
(require 'package)

(progn
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize)

  (when (not (package-installed-p 'req-package))
    (package-refresh-contents)
    (package-install 'req-package)))

(defun python-find-env (project-root)
  (let ((env-path (f-join project-root "env")))
    (when (f-exists? env-path)
      env-path))
  )

(let (python-current-env)
 (add-hook 'python-mode-hook
	  (lambda ()
	    (let* ((root (projectile-project-root))
		   (env (python-find-env root)))
	      (when (and env
			 (not (equal env
				     python-current-env)))
		(setf python-current-env env)
		(message "Current python env: %s" python-current-env)
		(pyvenv-activate env))))))

(defun async-gtags-update ()
  (call-process "global" nil 0 nil "-u"))

(defvar *gtags-modes*
  '(web-mode
    php-mode
    cperl-mode))

(add-hook 'after-save-hook
	  (lambda ()
	    (if (cl-member major-mode *gtags-modes*)
		(async-gtags-update))))

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

(add-hook 'after-save-hook
	  'whitespace-cleanup)

(setq browse-url-browser-function 'browse-url-firefox)

(progn
  (defalias 'perl-mode 'cperl-mode)
  (setq cperl-electric-parens nil
	cperl-electric-keywords nil
	cperl-electric-lbrace-space nil))

(setq backup-directory-alist
      '((".*" . "/home/juiko/.emacs.d/cache/"))
      auto-save-file-name-transforms
      '((".*" "/home/juiko/.emacs.d/cache/" t))
      auto-save-list-file-prefix
      "/home/juiko/.emacs.d/cache/")

(require 'req-package)

(req-package f)

(req-package erc
  :commands erc)

(req-package company
  :config (progn
	    (setq company-minimum-prefix-length 3)
	    (setq company-show-numbers t)
	    (global-company-mode)))

(req-package company-quickhelp
  :disabled t
  :require company
  :config (add-hook 'company-mode-hook 'company-quickhelp-mode))

(req-package flycheck
  :config (progn
	    (setf flycheck-perlcritic-severity 5)
	    (setf flycheck-ghc-args (list
				     "-fwarn-tabs"
				     "-fwarn-type-defaults"
				     "-fwarn-unused-do-bind"
				     "-fwarn-incomplete-uni-patterns"
				     "-fwarn-incomplete-patterns"
				     "-fwarn-incomplete-record-updates"
				     "-fwarn-monomorphism-restriction"
				     "-fwarn-auto-orphans"
				     "-fwarn-implicit-prelude"
				     "-fwarn-missing-exported-sigs"
				     "-fwarn-identities"
				     "-Wall"))
	    (global-flycheck-mode)))

(req-package flycheck-pos-tip
  :require flycheck
  :config (progn
	    (setf flycheck-display-errors-function
		  #'flycheck-pos-tip-error-messages)))

(req-package smartparens
  :config (progn
	    (sp-local-pair '(emacs-lisp-mode
			     lisp-mode
			     slime-repl-mode)
			   "`" nil :actions nil)
	    (sp-local-pair '(emacs-lisp-mode
			     lisp-mode
			     slime-repl-mode)
			   "'" nil :actions nil)
	    (add-to-list 'sp-no-reindent-after-kill-modes 'haskell-mode)
	    (smartparens-global-strict-mode)))


(req-package slime-company
  :require company)

(req-package slime
  :require slime-company
  :commands slime
  :init (progn
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

(req-package evil-lisp-state
  :require evil evil-leader
  :init (progn
	  (setq evil-lisp-state-global t)
	  (setq evil-lisp-state-enter-lisp-state-on-command nil))
  :config (progn
	    (evil-leader/set-key "L" 'evil-lisp-state)))

(req-package evil-smartparens
  :require evil smartparens
  :config (progn
	    (add-hook 'smartparens-strict-mode-hook 'evil-smartparens-mode)))

(req-package evil-commentary
  :require evil
  :config (progn
	    (evil-commentary-mode)
	    ))

(req-package evil-god-state
  :require evil god-mode
  :config (progn
	    (bind-key "ESC" 'evil-normal-state evil-god-state-map)))

(req-package evil-leader
  :require evil
  :config (progn
	    (setq evil-leader/leader (kbd ","))
	    (evil-leader/set-key
	      "f" 'helm-find-files
	      "b" 'switch-to-buffer
	      "g" 'helm-M-x
	      "k" 'kill-buffer
	      "," 'evil-execute-in-emacs-state
	      ";" 'comment-dwim
	      "e" 'eval-last-sexp
	      "w" 'save-buffer
	      "." 'ggtags-find-tag-dwim
	      "hs" 'helm-swoop
	      "ha" 'helm-ag
	      "hi" 'helm-semantic-or-imenu
	      "hP"  'helm-projectile
	      "hpa" 'helm-projectile-ag
	      "ptp" 'projectile-test-project
	      "mgb" 'magit-branch
	      "mgc" 'magit-checkout
	      "mgc" 'magit-checkout
	      "mgl" 'magit-log
	      "mgs" 'magit-status
	      "mgpl" 'magit-pull
	      "mgps" 'magit-push)

	    (evil-leader/set-key-for-mode 'haskell-mode "H" 'haskell-hoogle)
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

(req-package evil-magit
  :require evil magit
  )

(defun juiko/look-config ()
  (blink-cursor-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode 1)
  (global-hl-line-mode 1)
  (show-paren-mode)
  (add-to-list 'default-frame-alist '(font . "Hack-9"))
  (add-to-list 'default-frame-alist '(cursor-color . "Gray")))

(req-package badwolf-theme
  :disabled t
  :config (progn
	    (load-theme 'badwolf t)
	    (juiko/look-config)))

(req-package greymatters-theme
  :config (progn
	    (add-hook 'after-init-hook
		      (lambda ()
			(load-theme 'greymatters t)
			(set-face-attribute 'fringe
					    nil
					    :background "#f9fbfd"
					    :foreground "#f9fbfd")))
	    (juiko/look-config)))

(req-package leuven-theme
  :disabled t
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

	    (helm-mode)))

(req-package helm-projectile
  :commands helm-mode
  :require helm projectile
  :config (progn
	    (helm-projectile-on)))

(req-package helm-ag
  :require helm
  :commands helm-ag)

(req-package helm-swoop
  :commands helm-swoop
  :require helm)

(req-package helm-grep-ack
  :require helm
  :commands helm-ack)

(req-package magit
  :init (progn
	  (setq magit-last-seen-setup-instructions "1.4.0")))

(req-package haskell-mode
  :config (progn
	    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
	    (add-hook 'haskell-mode-hook 'haskell-doc-mode)
	    (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
	    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
	    (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
	    (add-hook 'haskell-mode-hook (lambda ()
					   (electric-indent-local-mode -1)))

	    (setq haskell-process-type 'stack-ghci)
	    (setq haskell-process-path-ghci "stack")
	    (setq haskell-process-args-ghci '("ghci"))

	    (setq haskell-process-suggest-remove-import-lines t)
	    (setq haskell-process-auto-import-loaded-modules t)
	    (setq haskell-process-log t)))


(req-package hindent
  :require haskell-mode
  :config (progn
	    (setq hindent-style "chris-done")
	    (evil-define-key 'evil-visual-state hindent-mode-map "TAB"
	      'hindent-reformat-region)
	    (add-hook 'haskell-mode-hook 'hindent-mode)))

(req-package flycheck-haskell
  :require flycheck haskell-mode
  :config (progn
	    (add-hook 'haskell-mode-hook 'flycheck-mode)
	    (add-hook 'flycheck-mode-hook 'flycheck-haskell-configure)))

(req-package company-ghci
  :require company haskell-mode
  :config (progn
	    (push 'company-ghci company-backends)))

(req-package anaconda-mode
  :config (progn
	    (add-hook 'python-mode-hook 'anaconda-mode)))

(req-package company-anaconda
  :require company anaconda-mode
  :config (progn
	    (add-to-list 'company-backends 'company-anaconda)))

(req-package pyvenv
  :config (progn
	    (add-hook 'python-mode-hook 'pyvenv-mode)))

(req-package yasnippet
  :config (progn
	    (yas-global-mode)))

(req-package irony
  :config (progn
	    (add-hook 'c-mode-hook 'irony-mode)
	    (add-hook 'c++-mode-hook 'irony-mode)
	    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(req-package company-irony
  :require company irony
  :commands irony-mode
  :config (progn
	    (push 'company-irony company-backends)))

(req-package flycheck-irony
  :require flyckeck irony
  :config (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

(req-package irony-eldoc
  :require irony
  :config (progn
	    (add-hook 'irony-mode-hook 'eldoc-mode)))


(req-package web-mode
  :config (progn
	    (add-hook 'web-mode-hook
		      (lambda ()
			(setq web-mode-enable-auto-pairing t)
			(setq web-mode-enable-css-colorization t)
			(setq web-mode-enable-block-face t)
			(setq web-mode-enable-heredoc-fontification t)
			(setq web-mode-enable-current-element-highlight nil)
			(setq web-mode-enable-current-column-highlight nil)
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
	    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
	    (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))))

(req-package php-mode
  :config (progn
	    (require 'php-ext)
	    (setq php-template-compatibility nil)))

(req-package js2-mode
  :config (progn
	    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))))

(req-package rust-mode
  )

(req-package racer
  :require rust-mode company
  :config (progn
	    (setq racer-cmd "/home/juiko/git/racer/target/release/racer")
	    (setq racer-rust-src-path "/home/juiko/git/rust/src/")
	    (add-hook 'rust-mode-hook 'racer-mode)
	    (add-hook 'racer-mode-hook 'eldoc-mode)
	    ))

(req-package flycheck-rust
  :require rust-mode flycheck
  :config (progn
	    (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)))

(req-package-finish)
