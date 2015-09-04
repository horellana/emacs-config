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
               '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  )

(package-initialize)

(condition-case ex
    (require 'req-package)
  ('error (progn
            (package-refresh-contents)
            (package-install "req-package")
            (require 'req-package))))

(cl-defun define-company-sources (mode-hook backend)
  (add-hook mode-hook
            (lambda ()
              (make-variable-buffer-local 'company-backends)
              (setq-local company-backends backend))))


(cl-defmacro with-packages (packages &body body)
  (if (= (length packages) 1)
      `(eval-after-load ,(car packages)
         ,@body)
    `(eval-after-load ,(car packages)
       (with-packages ,(cdr packages) ,@body))))




(req-package go-mode)

(req-package company-go
  :require (go-mode company)
  :config (progn
            (define-company-sources 'go-mode-hook
              (list 'company-go))
            (add-hook 'go-mode-hook 'company-mode)))

(req-package multiple-cursors)

(req-package rx)

(req-package tex
  :commands (tex-mode)
  :config (progn
            (setq TeX-auto-save t)
            (setq TeX-parse-self t)
            (setq TeX-PDF-mode t)
            (setq-default TeX-master nil)
            (setq TeX-view-program-selection
                  '((output-pdf "PDF Viewer")))
            (setq TeX-view-program-list
                  '(("PDF Viewer" "okular --unique %o#src:%n%b")))))

(req-package company-auctex
  :require (company)
  :config (progn
            (add-hook 'TeX-mode-hook
                      (lambda ()
                        (make-variable-buffer-local 'company-backends)
                        (company-auctex-init)
                        (company-mode)))))

(req-package emacs-eclim
  :disabled t
  :config (progn
            (eval-after-load "company" (company-emacs-eclim-setup))
            (global-eclim-mode)))

(req-package disaster
  :commands (disaster)
  :bind (("C-c d" . disaster)))

(req-package focus
  :disabled t
  :config (progn
            (add-hook 'lisp-mode-hook 'focus-mode)
            ;; (add-hook 'emacs-lisp-mode-hook 'focus-mode)
            ))

(req-package eldoc
  :config (progn
            (add-hook 'ielm-mode-hook 'eldoc-mode)
            (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
            ))

(req-package flycheck
  :config (progn
            (custom-set-variables
             '(flycheck-display-errors-function 
               #'flycheck-pos-tip-error-messages))
            
            (cl-loop for hook in '(go-mode-hook
                                   clojure-mode-hook
                                   TeX-mode-hook
                                   python-mode-hook
                                   rust-mode-hook
                                   enh-ruby-mode-hook
                                   python-hook
                                   cperl-mode-hook
                                   emacs-lisp-mode-hook
                                   c-mode-common-hook)
                     do (add-hook hook 'flycheck-mode))))

(req-package flycheck-pos-tip
  :require (flycheck)
  :commands (flycheck-mode))

(req-package flycheck-package
  :require (flycheck)
  :commands (flycheck-mode)
  :config (progn
            (flycheck-package-setup)))

(req-package flycheck-haskell
  :require (haskell-mode flycheck)
  :config (progn
            (add-hook 'haskell-mode-hook 'flycheck-mode)
            (add-hook 'flycheck-mode-hook 'flycheck-haskell-configure)))

(req-package ghc
  :disabled t
  :config (progn
            (autoload 'ghc-init "ghc" nil t)
            (autoload 'ghc-debug "ghc" nil t)
            (add-hook 'haskell-mode-hook 'ghc-init)))

(req-package company-ghc
  :disabled t
  :require (ghc)
  )

(req-package company-ghci
  :require (company))

(req-package haskell-mode
  :commands haskell-mode
  :require (company company-ghci)
  :config (progn
            (define-company-sources 'haskell-mode-hook
              (list 'company-ghci))
            
            (define-company-sources 'haskell-interative-mode-hook
              (list 'company-ghci))

            (add-hook 'haskell-mode-hook 'company-mode)
            (add-hook 'haskell-interactive-mode-hook 'company-mode)
            
            (bind-key "C-c C-l" 'haskell-process-load-or-reload haskell-mode-map)
            (add-hook 'haskell-mode-hook 'turn-on-haskell-doc)
            (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
            (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
            (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
            (setq haskell-process-type 'stack-ghci)
            (setq haskell-process-path-ghci "stack")
            (setq haskell-process-args-ghci "ghci")
            ))

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

(req-package common-lisp-snippets
  :require (yasnippet)
  :commands (lisp-mode))

(req-package evil-lisp-state
  :require (evil)
  :config (progn
            (define-key evil-normal-state-map (kbd "L") 'evil-lisp-state)))

(req-package evil
  :config (progn
            (define-key evil-normal-state-map [escape] 'keyboard-quit)
            (define-key evil-visual-state-map [escape] 'keyboard-quit)
            (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
            (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
            (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
            (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
            (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
            (define-key evil-visual-state-map (kbd "TAB") 'indent-region)
            (define-key evil-normal-state-map (kbd "C-TAB") 'indent-whole-buffer)
            (define-key evil-normal-state-map [return]
              (lambda ()
                (interactive)
                (save-excursion
                  (newline))))

            (defadvice eval-last-sexp (around evil)
              "Last sexp ends at point."
              (when (evil-normal-state-p)
                (save-excursion
                  (unless (or (eobp) (eolp)) (forward-char))
                  ad-do-it)))

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
            
            (setf evil-move-cursor-back nil)
            (evil-mode t)))

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

(req-package evil-god-state
  :require (evil god-mode)
  :config (progn
            (bind-key "ESC" 'evil-normal-state evil-god-state-map)))

(req-package company
  :config (progn
            (add-hook 'php-mode-hook 'company-mode)
            (add-hook 'ielm-mode-hook 'company-mode)
            (add-hook 'cider-repl-mode-hook 'company-mode)
            (add-hook 'cider-mode-hook 'company-mode)
            (add-hook 'rust-mode-hook 'company-mode)
            (add-hook 'c-mode-common-hook 'company-mode)
            (add-hook 'emacs-lisp-mode-hook 'company-mode)
            (add-hook 'inferior-emacs-lisp-mode-hook 'company-mode)
            (add-hook 'python-hook 'company-mode)
            (setq company-idle-delay 0.5)))


(req-package irony
  :disabled t
  :config (progn
            (add-hook 'c-mode-hook 'irony-mode)
            (add-hook 'c++-mode-hook 'irony-mode)
            (defun my-irony-mode-hook ()
              (define-key irony-mode-map [remap completion-at-point]
                'irony-completion-at-point-async)
              (define-key irony-mode-map [remap complete-symbol]
                'irony-completion-at-point-async))
            (add-hook 'irony-mode-hook 'my-irony-mode-hook)
            (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(req-package company-irony
  :require (company irony)
  :config (progn
            (add-hook 'c++-mode-hook 
                      (lambda ()
                        (make-variable-buffer-local 'company-backends)
                        (setq company-backends '(company-irony))))
            (add-hook 'c-mode-hook 
                      (lambda ()
                        (make-variable-buffer-local 'company-backends)
                        (setq company-backends '(company-irony))))
            (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)))

(req-package irony-eldoc
  :require (irony eldoc)
  :config (progn
            (add-hook 'irony-mode-hook 'eldoc-mode)
            (add-hook 'irony-mode-hook 'irony-eldoc)))

(req-package flycheck-irony
  :require (flycheck irony)
  :config (progn
            (add-hook 'irony-mode-hook 'flycheck-irony-setup)))

(req-package ggtags
  :requires (cperl-mode)
  :commands (c++-mode c-mode cperl-mode)
  :config (progn
            (add-hook 'c-mode-common-hook 'ggtags-mode)))

(req-package aggressive-indent
  :config (progn
            (add-hook 'lisp-mode-hook 'aggressive-indent-mode)
            (add-hook 'web-mode-hook 'aggressive-indent-mode)
            (add-hook 'js2-mode-hook 'aggressive-indent-mode)
            (add-hook 'cperl-mode-hook 'aggressive-indent-mode)
            (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
            (add-hook 'c-mode-common-hook 'aggressive-indent-mode)))

(req-package company-c-header
  :require (company)
  :commands (company-mode))

(req-package erc
  :commands (erc start-erc)
  :config (progn
            (erc-autojoin-mode 1)
            
            (setq erc-autojoin-channels-alist
                  '(("freenode.net"
                     "#emacs"
                     "#gentoo"
                     "#stumpwm"
                     "#haskell"
                     "#concatenative"
                     "#perl"
                     "#lisp"
                     "#programadoreschile"
                     "#archlinux")
                    ("torncity.com"
                     "#lobby"
                     "#help")
                    ("perl.org"
                     "#p5p")
                    ("rizon.io"
                     "#/r/syriancivilwar")))

            (defun start-erc ()
              (interactive)
              (erc :nick "juiko" :password "caballovolado"))
            ))

(req-package factor-mode
  :load-path "/home/juiko/git/factor/misc/fuel"
  :require (fuel-mode)
  :config (progn
            (let ((root-dir "/home/juiko/git/factor"))
              (setf factor-root-dir root-dir
                    fuel-listener-factor-binary (concat factor-root-dir
                                                        "/"
                                                        "factor")
                    
                    fuel-listener-factor-image (concat factor-root-dir
                                                       "/"
                                                       "factor.image")

                    factor-indent-level 2))))

(req-package fuel-mode
  :load-path "/home/juiko/git/factor/misc/fuel"
  :config (progn
            (add-hook 'factor-mode-hook 'fuel-mode)))

(defun juiko/look-config ()
  (blink-cursor-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode 1)
  (global-hl-line-mode 1)

  (add-to-list 'default-frame-alist '(font . "Droid Sans Mono-9"))
  (add-to-list 'default-frame-alist '(cursor-color . "Gray")))

(req-package monokai-theme
  :disabled t
  :config (progn
            (load-theme 'monokai t)
            (juiko/look-config)))

(req-package material-theme
  :disabled t
  :config (progn
            (load-theme 'material-light t)
            (set-face-attribute 'fringe
                                nil
                                :background "#FAFAFA"
                                :foreground "#FAFAFA")
            (setq popup-tip-face "#C0C0C0")
            (juiko/look-config)))

(req-package leuven-theme
  :config (progn
            (load-theme 'leuven t)
            (set-face-attribute 'fringe
                                nil
                                :background "2e3436"
                                :foreground "2e3436")
            (juiko/look-config)))

(req-package helm-descbinds
  :require (helm-config)
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

(req-package helm-projectile
  :require (helm-config projectile)
  :config (progn
            (helm-projectile-on)))

(req-package helm-company
  :require (helm-config company)
  :commands (company-mode)
  :config (progn
            (bind-key "C--" 'helm-company company-mode-map)
            (bind-key "C--" 'helm-companycompany-active-map)))


(req-package helm-gtags
  :require (helm-config)
  :commands (helm-gtags)
  :config (progn
            (setq
             helm-gtags-ignore-case t
             helm-gtags-auto-update t
             helm-gtags-use-input-at-cursor t
             helm-gtags-pulse-at-cursor t
             helm-gtags-prefix-key "\C-cg"
             helm-gtags-suggested-key-mapping t)))


(req-package helm-config
  :config (progn
            (bind-key "C-h a" 'helm-apropos)
            (bind-key "C-x C-b" 'switch-to-buffer)

            (add-to-list 'display-buffer-alist
                         `(,(rx bos "*helm" (* not-newline) "*" eos)
                           (display-buffer-in-side-window)
                           (inhibit-same-window . t)
                           (window-height . 0.4)))

            (helm-mode t)))

(req-package helm-ag
  :commands (helm-ag)
  :require (helm-config))

(req-package helm-ack
  :commands (helm-ack)
  :require (helm-config)
  :ensure t)

(req-package helm-swoop
  :commands (helm-swoop)
  :require (helm-config)
  :ensure t)


(req-package magit
  :commands (magit-status magit-init magit-log)
  :defer t
  :init (progn
          (setq magit-last-seen-setup-instructions "1.4.0")))

(req-package cperl-mode
  :init (progn
          (defalias 'perl-mode 'cperl-mode))
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.t\\'" . cperl-mode))
            (add-hook 'cperl-mode-hook
                      (lambda ()
                        (cperl-set-style "K&R")
                        (setq 
                         cperl-indent-level 2)))
            (add-hook 'cperl-mode-hook
                      (lambda ()
                        (setq-local ac-sources 
                                    '(ac-source-abbrev
                                      ac-source-filename
                                      ac-source-words-in-buffer
                                      ac-source-words-in-same-mode-buffers))))))

(req-package plsense
  :disabled t
  :require (cperl-mode)
  :commands (cperl-mode)
  :config (progn
            (add-hook 'cperl-mode-hook
                      '(lambda ()
                         (setq plsense-popup-help "C-c C-:")
                         (setq plsense-display-help-buffer "C-c M-:")
                         (setq plsense-jump-to-definition "C-c C->")
                         (plsense-config-default)))))

(req-package yasnippet
  :commands (yas-minor-mode)
  :init (progn
          (add-hook 'lisp-mode-hook 'yas-minor-mode)
          (add-hook 'TeX-mode-hook 'yas-minor-mode)
          (add-hook 'cperl-mode-hook 'yas-minor-mode)))

(req-package projectile
  :config (progn
            (setq projectile-indexing-method 'alien)
            (setq projectile-enable-caching t)
            (projectile-global-mode)))

(req-package socks
  :defer t
  :config (progn
            (setq url-proxy-services
                  '(("no_proxy" . "^\\(localhost\\|10.*\\)")
                    ("http" . "localhost:8118")
                    ("https" . "localhost:8118")))))

(req-package python
  :config (progn
            (setq python-shell-interpreter "ipython")
            (add-hook 'python-mode-hook
                      (lambda ()
                        (highlight-indentation-mode -1)))))

(req-package elpy
  :require (python)
  :disabled t
  :config (progn
            (setq elpy-rpc-backend "jedi")
            (elpy-enable)
            (elpy-use-ipython)
            (add-hook 'inferior-python-mode-hook 'company-mode)))

(req-package pyvenv
  :require (python)
  :config (progn
            (add-hook 'python-mode-hook 'pyvenv-mode)))

(req-package anaconda-mode
  :config (progn
            (add-hook 'python-mode-hook 'anaconda-mode)
            (add-hook 'python-mode-hook 'eldoc-mode)))

(req-package company-anaconda
  :require (company anaconda-mode)
  :config (progn
            (add-hook 'python-mode-hook
                      (lambda ()
                        (make-variable-buffer-local 'company-backends)
                        (setq-local company-backends '(company-anaconda))))
            (add-hook 'python-mode-hook 'company-mode)))

(req-package virtualenvwrapper
  :disabled t
  :commands (python)
  :config (progn
            (venv-initialize-interactive-shells)
            (venv-initialize-eshell)))

(req-package enh-ruby-mode
  :mode "\\.rb\\'")

(req-package inf-ruby
  :require (enh-ruby-mode)
  :config (progn
            (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)))

(req-package robe
  :require (enh-ruby-mode)
  :config (progn
            (add-hook 'enh-ruby-mode-hook 'robe-mode)
            (add-hook 'robe-mode-hook 'ac-robe-setup)))

(req-package ruby-end
  :require (enh-ruby-mode)
  :init(progn
         (add-hook 'enh-ruby-mode-hook 'ruby-end-mode)))

(req-package re-builder
  :defer t
  :config (progn
            (setq reb-re-syntax 'string)))

(req-package rust-mode
  :mode "\\.rs\\'")

(req-package company-racer
  :require (company rust-mode)
  :config (progn
            (custom-set-variables 
             '(company-racer-rust-executable
               "/home/juiko/git/racer/target/release/racer"))

            (add-hook 'rust-mode-hook
                      (lambda ()
                        (make-variable-buffer-local 'company-backends)
                        (setq-local company-backends '(company-racer))))))

(req-package flycheck-rust
  :require (flycheck rust-mode)
  :config (progn
            (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)))

(req-package geiser
  :require (company)
  :commands (scheme-mode geiser-mode)
  :config (progn
            (add-hook 'geiser-mode 'geiser-autodoc-mode)
            (add-hook 'scheme-mode 'turn-on-geiser-mode)
            (add-hook 'geiser-mode-hook 'company-mode)))

(req-package smooth-scrolling
  :disabled t)

(req-package web-mode
  :commands (web-mode auto-complete)
  :init (progn
          (defalias 'html-mode 'web-mode)
          (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
          (add-to-list 'auto-mode-alist '("\\.html" . web-mode)))
  :config (progn
            (setq web-mode-ac-sources-alist
                  '(("css" . (ac-source-css-property))
                    ("css" . (ac-source-words-in-buffer
                              ac-source-words-in-same-mode-buffers
                              ac-source-abbev))
                    ("html" . (ac-source-words-in-buffer 
                               ac-source-words-in-same-mode-buffers
                               ac-source-abbrev))))))

(req-package js2-mode
  :commands (js2-mode javascript-mode)
  :init (progn
          (defalias 'javascript-mode 'js2-mode)))

(req-package ac-js2
  :require (auto-complete)
  :config (progn
            (add-hook 'js2-mode-hook 'ac-js2-mode)))

(req-package rainbow-mode
  :require (web-mode js2-mode)
  :mode "\\.js2\\'"
  :config (progn
            (add-hook 'web-mode-hook 'rainbow-mode)
            (add-hook 'html-mode-hook 'rainbow-mode)
            (add-hook 'css-mode-hook 'rainbow-mode)
            (add-hook 'js2-mode-hook 'rainbow-mode)))

(req-package whitespace
  :defer 5
  :config (progn
            (setq whitespace-line-column 80)
            (setq whitespace-style '(face lines-tail))
            (add-hook 'prog-mode-hook 'whitespace-mode)))

(req-package smartparens-config
  :config (progn
            (dolist (hook '(lisp-mode-hook
                            clojure-mode-hook
                            slime-repl-mode-hook
                            ielm-mode-hook
                            emacs-lisp-mode-hook
                            ))
              (add-hook hook 'smartparens-strict-mode))))

(req-package smartparens-haskell
  :require (smartparens-config haskell-mode))

(req-package smartparens-python
  :require (smartparens-config python))

(req-package evil-smartparens
  :require (evil smartparens)
  :config (progn
            (add-hook 'smartparens-mode-hook #'evil-smartparens-mode)
            (evil-sp-override)))

(req-package evil-commentary
  :require (evil)
  :config (progn
            (evil-commentary-mode)
            (evil-commentary-default-setup)))

(req-package auto-complete
  :config (progn
            (add-hook 'cperl-mode-hook 'auto-complete-mode)
            (add-hook 'web-mode-hook 'auto-complete-mode)))

(req-package cider
  :pin melpa-stable
  :config (progn
            (add-hook 'clojure-mode-hook 'cider-mode)
            (add-hook 'cider-mode-hook 'eldoc-mode)
            (setq cider-repl-pop-to-buffer-on-connect nil)))


(req-package cider-decompile
  :require (cider))

(req-package flycheck-clojure
  :require (cider flycheck)
  :config (progn
            (add-hook 'clojure-mode-hook 'flycheck-clojure-setup)
            (add-hook 'clojure-mode-hook 'flycheck-mode)))

(req-package-finish)

;;;;;;;;;;;;;; Personal configuration ;;;;;;;;;;;;;;;;;;;;

(add-hook 'highlight-parentheses-mode-hook
          '(lambda ()
             (setq autopair-handle-action-fns
                   (append
                    (if autopair-handle-action-fns
                        autopair-handle-action-fns
                      '(autopair-default-handle-action))
                    '((lambda (action pair pos-before)
                        (hl-paren-color-update)))))))

(defun compile-everything (list)
  (dolist (folder list) (byte-recompile-directory folder 0)))

(defun compile-load-path ()
  (interactive)
  (compile-everything load-path))

(defun clear-screen ()
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max))))

(defun buffer-empty-p ()
  (<= (point-max) 1))

(defun indent-whole-buffer ()
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max))
  (untabify (point-min) (point-max)))

(defun byte-compile-user-init-file ()
  (let ((byte-compile-warnings '(unresolved)))
    (when (file-exists-p (concat user-init-file ".elc"))
      (delete-file (concat user-init-file ".elc")))
    (byte-compile-file user-init-file)
    (message "%s compiled" user-init-file)))

(defun remove-files (files)
  (dolist (f files)
    (delete-file f)))

(defun safe-load-path ()
  (cl-remove-if-not (lambda (x) (string-match (expand-file-name "~") x))
                    load-path))

(defun get-compiled-files (files)
  (cl-remove-if-not (lambda (x) (string-match "\\(\\.elc\\)" x)) files))

(defun remove-compiled-files (files)
  (remove-files (get-compiled-files files)))

(defun directory-files-full-path (directory)
  (mapcar (lambda (x) (concat directory "/" x)) (directory-files directory)))

(defun decompile-load-path ()
  (dolist (dir load-path)
    (remove-compiled-files (directory-files-full-path dir))))

(defun kill-buffer-match-regexp (pattern)
  (interactive "sRegex: ")
  (dolist (buffer (buffer-list))
    (when (string-match pattern (buffer-name buffer))
      (kill-buffer buffer))))

(defun git-etags-create-command (tags-file &optional lang)
  (format "git ls-files | xargs %s"
          (if lang
              (format "etags --output=%s --language=%s" tags-file lang)
            (format "etags --output=%s" tags-file))))

(defun git-etags-find-tags-file ()
  (format "%s/.TAGS"
          (substring
           (shell-command-to-string "git rev-parse --show-toplevel")
           0 -1)))

(defun git-etags-create (&optional lang)
  (interactive)
  (ignore-errors
    (let ((tags-file (git-etags-find-tags-file))
          (output-buffer "*git-etags-create-OUTPUT*")
          (error-buffer "*git-etags-create-ERROR*"))
      (shell-command
       (git-etags-create-command tags-file lang)
       output-buffer error-buffer)
      tags-file)))

(defun proc (name &optional args)
  (let ((process-connection-type nil))
    (if args
        (start-process name (format "%s-buffer" name) name args)
      (start-process name (format "%s-buffer" name) name))))


(defun intersperce (n l)
  (cl-reduce (lambda (a b) (concat a n b)) l))

(progn
  (setq-default lexical-binding t)
  (setq password-cache t
        password-cache-expiry nil
        tramp-default-method "ssh")
  (show-paren-mode)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq-default standard-indent 2)
  (setq-default lisp-body-indent 2)

  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  
  (add-hook 'after-save-hook
            (lambda ()
              (let ((init-file (expand-file-name "~/.emacs.d/init.el")))
                (when (equal (buffer-file-name) init-file)
                  (byte-compile-file init-file)))))

  (setq inhibit-startup-message t)
  (global-set-key (kbd "C-<tab>") 'indent-whole-buffer)
  (global-set-key (kbd "M--") 'hippie-expand)
  (global-set-key (kbd "C-.") 'clear-screen)
  (global-set-key (kbd "C-;") 'comment-dwim)

  (global-set-key (kbd "M-g M-g") 
                  '(lambda ()
                     (interactive)
                     (unwind-protect
                         (progn
                           (linum-mode t)
                           (call-interactively 'goto-line))
                       (linum-mode -1))))

  (global-set-key (kbd "M-<left>") 'enlarge-window-horizontally)
  (global-set-key (kbd "M-<down>") 'enlarge-window-vertically)
  (global-set-key (kbd "M-<right>") 'shrink-window-horizontally)
  (global-set-key (kbd "C-%") 'iedit-mode)

  (fset 'yes-or-no-p 'y-or-n-p)

  (global-auto-revert-mode 1)

  (electric-indent-mode t)

  ;; (recentf-mode t)
  (windmove-default-keybindings)

  ;; I dont like temp files
  ;; hide them !
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))

  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)

  (setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
   '(("." . "~/.emacs.d/saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t))

(put 'downcase-region 'disabled nil)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("9e1e2e7590c2f443c09a3f6240a05ada53f06566a6873c37eeae10d13dc372c9" default)))
 '(fci-rule-color "#49483E")
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(highlight-changes-colors ("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   (quote
    (("#49483E" . 0)
     ("#67930F" . 20)
     ("#349B8D" . 30)
     ("#21889B" . 50)
     ("#968B26" . 60)
     ("#A45E0A" . 70)
     ("#A41F99" . 85)
     ("#49483E" . 100))))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (web-mode virtualenvwrapper smooth-scrolling slime-company rust-mode ruby-end robe req-package rainbow-mode plsense php-mode multiple-cursors monokai-theme material-theme magit leuven-theme irony-eldoc helm-swoop helm-projectile helm-gtags helm-descbinds helm-company helm-ag helm-ack ggtags geiser focus flycheck-rust flycheck-pos-tip flycheck-package flycheck-irony flycheck-haskell flycheck-clojure evil-smartparens evil-lisp-state evil-god-state evil-commentary enh-ruby-mode emacs-eclim elpy disaster company-racer company-irony company-ghci company-auctex common-lisp-snippets cider-decompile aggressive-indent ac-js2)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun vim-macros ()
  (interactive)
  (eww-browse-url 
   "http://www.thegeekstuff.com/2009/01/vi-and-vim-macro-tutorial-how-to-record-and-play/"))
