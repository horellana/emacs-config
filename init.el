;; -*- lexical-binding: t -*-

(eval-when-compile
  (require 'cl))

(require 'package)

(progn
  (setq package-enable-at-startup nil)

  (add-to-list 'package-archives 
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/") t)

  (add-to-list 'package-archives
               '("melpa-stable" . "http://stable.melpa.org/packages/") t))

(package-initialize)
(require 'req-package)

(req-package company
  :config (progn
            (global-company-mode)))

(req-package flycheck
  :config (progn
            (custom-set-variables
             '(flycheck-display-errors-function 
               #'flycheck-pos-tip-error-messages))
            (global-flycheck-mode)))

(req-package flycheck-pos-tip
  :require (flycheck)
  :commands (flycheck-mode))

(req-package smartparens
  :config (progn
            (smartparens-global-strict-mode)))

(req-package slime
  :init (progn
          (setq inferior-lisp-program "/home/juiko/git/sbcl/run-sbcl.sh")
          ;; (setq inferior-lisp-program "sbcl")
          (setq slime-contrib '(slime-fancy
                                slime-company))
          (setq slime-sbcl-manual-root "/usr/local/share/info/sbcl.info")
          (add-hook 'lisp-mode-hook
                    (lambda ()
                      (unless (slime-connected-p)
                        (save-excursion (slime))))))
  :config (progn
            (slime-setup '(slime-fancy slime-company))))

(defun cl-config/configure-company-slime ()
  (make-variable-buffer-local 'company-backends)
  (setq-local company-backends '()))

(req-package slime-company
  :require (slime company)
  :commands (slime slime-mode slime-repl-mode)
  :config (progn
            (add-hook 'slime-mode-hook 
                      'cl-config/configure-company-slime)
            (add-hook 'slime-interactive-mode-hook 
                      'cl-config/configure-company-slime)))

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

	    (evil-mode t)))

(req-package evil-smartparens
  :require (evil smartparens)
  :config (progn
            (add-hook 'smartparens-strict-mode-hook 'evil-smartparens-mode)))

(req-package evil-commentary
  :require (evil)
  :config (progn
	    (evil-commentary-default-setup)
            (evil-commentary-mode)))

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
              "hi" 'helm-semantic-or-imenu)
            (global-evil-leader-mode)
            
            (evil-leader/set-key-for-mode 'emacs-lisp-mode "ma" 'pp-macroexpand-last-sexp)
            (evil-leader/set-key-for-mode 'lisp-interaction-mode "ma" 'pp-macroexpand-last-sexp)

            (evil-leader/set-key-for-mode 'lisp-mode "cl" 'slime-load-file)
            (evil-leader/set-key-for-mode 'lisp-mode "e" 'slime-eval-last-expression)
            (evil-leader/set-key-for-mode 'lisp-mode "me" 'slime-macroexpand-1)
            (evil-leader/set-key-for-mode 'lisp-mode "ma" 'slime-macroexpand-all)
            (evil-leader/set-key-for-mode 'lisp-mode "sds" 'slime-disassemble-symbol)
            (evil-leader/set-key-for-mode 'lisp-mode "sdd" 'slime-disassemble-definition)
            (evil-leader/set-key-for-mode 'cider-mode "e" 'cider-eval-last-sexp)
            (evil-leader/set-key-for-mode 'projectile-mode (kbd "p")'helm-projectile)))

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
            (load-theme 'leuven t)
            (set-face-attribute 'fringe
                                nil
                                :background "2e3436"
                                :foreground "2e3436")
            (juiko/look-config)))

(req-package projectile)

(req-package helm
  :config (progn
	    (require 'helm-config)
	    (add-to-list 'display-buffer-alist
                         `(,(rx bos "*helm" (* not-newline) "*" eos)
                           (display-buffer-in-side-window)
                           (inhibit-same-window . t)
                           (window-height . 0.4)))
	    (helm-mode t)))

(req-package helm-projectile
  :require (helm projectile))

(req-package helm-swoop
  :commands (helm-swoop)
  :require (helm-config))

(req-package helm-grep-ack
  :require (helm)
  :commands (helm-ack))

(req-package magit
  :defer t
  :init (progn
	  (setq magit-last-seen-setup-instructions "1.4.0")))

(req-package haskell-mode
  :config (progn
            (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
            (add-hook 'haskell-mode-hook 'haskell-doc-mode)
            (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
            (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
            (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
            
	    (setq haskell-process-type 'stack-ghci)
            (setq haskell-process-path-ghci "stack")
            (setq haskell-process-args-ghci "ghci")

	    (setq haskell-process-suggest-remove-import-lines t)
	    (setq haskell-process-auto-import-loaded-modules t)
	    (setq haskell-process-log t)))

(req-package flycheck-haskell
  :require (flycheck haskell-mode)
  :config (progn
            (add-hook 'haskell-mode-hook 'flycheck-mode)
            (add-hook 'flycheck-mode-hook 'flycheck-haskell-configure)))

(req-package company-ghci
  :require (company haskell-mode)
  :config (progn
	    (push 'company-ghci company-backends)))

(req-package anaconda-mode
  :config (progn
	    (add-hook 'python-mode-hook 'eldoc-mode)
	    (add-hook 'python-mode-hook 'anaconda-mode)))

(req-package company-anaconda
  :config (progn
	    (push 'company-anaconda company-backends)))

(req-package pyvenv
  :config (progn
	    (add-hook 'python-mode-hook 'pyvenv-mode)))

(req-package yasnippet
  :config (progn
	    (yas-global-mode 1)))

(req-package-finish)

(electric-indent-mode t)
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
