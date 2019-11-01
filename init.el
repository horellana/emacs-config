; -*- lexical-binding: t -*-

(setq lexical-binding t)

(eval-when-compile
  (require 'cl)
  )

(require 'package)

;; (when (string-match-p "24" emacs-version)
  (package-initialize);;)

(progn
  (setq package-archives
        '(("melpa" . "https://melpa.org/packages/")
          ("gnu" . "https://elpa.gnu.org/packages/")
          ("org" . "https://orgmode.org/elpa/"))))
(when (or (not (package-installed-p 'use-package))
          (not (package-installed-p 'req-package)))
  (package-refresh-contents)
  (package-install 'el-get)
  (package-install 'use-package)
  (package-install 'req-package))

(setq-default tab-width 2)
(setq-default tramp-default-method "ssh")
(setq-default indent-tabs-mode nil)
(setq-default inhibit-startup-message t)
(setq browse-url-generic-program "firefox")
(setq browse-url-browser-function 'browse-url-firefox)
(setq ring-bell-function 'ignore)
(setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
(setq-default backup-by-copying t      ; don't clobber symlinks
              backup-directory-alist '(("." . "~/.emacs.d/saves"))    ; don't litter my fs tree
              delete-old-versions t
              kept-new-versions 6
              kept-old-versions 2
              version-control t)

(setf tab-width 2)

(progn
  (defalias 'perl-mode 'cperl-mode)
  (eval-after-load "cperl-mode"
    '(progn
       (setq-default cperl-electric-parens nil
                     cperl-electric-keywords nil
                     cperl-electric-lbrace-space nil))))

(set-face-attribute 'fringe nil
                    :foreground (face-foreground 'default)
                    :background (face-background 'default))

(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'ruby-mode-hook 'eldoc-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'doc-view-minor-mode-hook 'auto-revert-mode)
(add-hook 'prog-mode-hook 'eldoc-mode)

(add-hook 'after-save-hook
          (lambda ()
            (let ((init-file (expand-file-name "~/.emacs.d/init.el")))
              (when (equal (buffer-file-name) init-file)
                (byte-compile-file init-file)))))

(defun windows-subsystem-linux-p ()
  "Return t if running in WSL."
  (not
   (null (string-match-p "Microsoft"
                         (shell-command-to-string "uname -a")))))

(setf js-indent-level 2)

(eval-after-load "projectile"
  '(eval-after-load "f"
     '(progn
        (defun gtags-exists-p (root)
          (require 'f)
          (-contains-p  (f-files root)
                        (f-join root "GTAGS")))

        (defun async-gtags-create ()
          (call-process "gtags" nil 0 nil))

        (defun async-gtags-update ()
          (call-process "global" nil 0 nil "-u"))

        (defun async-gtags (root)
          (if (gtags-exists-p root)
              (async-gtags-update)
            (let ((default-directory root))
              (async-gtags-create))))

        (defvar *gtags-modes*)
        (setf *gtags-modes*
              '(web-mode
                php-mode
                cperl-mode
                ruby-mode
                ))


        (add-hook 'after-save-hook
                  (lambda ()
                    (require 'projectile)
                    (let ((generate-tags-p (member major-mode *gtags-modes*))
                          (project-root (projectile-project-root)))

                      (when (and generate-tags-p
                                 project-root)
                        (async-gtags project-root))))))))

(eval-after-load "python"
  '(progn
     (add-hook 'inferior-python-mode-hook
               (lambda ()
                 (python-shell-send-string "__name__ = None")))

     (add-hook 'after-save-hook
               (lambda ()
                 (when (eq major-mode 'python-mode)
                   (let ((process (python-shell-get-process)))
                     (when process
                       (python-shell-send-file (buffer-file-name (current-buffer))
                                               process))))))))

(eval-after-load "org"
  '(progn
     (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))))

(set-frame-font "Hack-10")

(eval-after-load "bind-key"
  '(progn
     (bind-key "<f8>" 'compile)))


(require 'req-package)

(use-package benchmark-init
  :disabled t
  :ensure t
  :config (add-hook 'after-init-hook 'benchmark-init/deactivate))

(progn
  (blink-cursor-mode -1)

  (when (not (eq system-type 'darwin))
    (menu-bar-mode -1))

  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode 1)
  (global-hl-line-mode 1)
  (show-paren-mode 1))

(use-package exec-path-from-shell
  :ensure t
  :if (eq system-type 'darwin)
  :config (progn
            (when (memq window-system '(mac ns x))
              (exec-path-from-shell-initialize))))

(use-package kooten-theme
  :ensure t
  :config (progn
            (load-theme 'kooten t)
            (set-face-attribute 'fringe nil :background nil)))

(use-package one-themes
  :ensure t
  :disabled t
  :config (progn
            (load-theme 'one-dark t)))

(use-package solarized-theme
  :disabled t
  :ensure t
  :config (progn
            (load-theme 'solarized-light t)))

(use-package leuven-theme
  :disabled t
  :ensure t
  :config (progn
            (load-theme 'leuven t)))

(use-package dracula-theme
  :disabled t
  :ensure t
  :config (progn
            (load-theme 'dracula t)))

(use-package tao-theme
  :disabled t
  :ensure t
  :config (progn
            (load-theme 'tao-yin)))

(use-package doom-themes
  :ensure t
  :disabled t
  :config (progn
            (doom-themes-visual-bell-config)
            (doom-themes-org-config)
            (load-theme 'doom-gruvbox t)))

(defun juiko/python-find-env (project-root)
  "Find the python project env directory, inside PROJECT-ROOT."
  (require 'projectile)
  (car (-intersection (mapcar (lambda (path) (f-join project-root path))
                              (list "env" ".env"))
                      (f-directories project-root))))

(req-package magit
  :defer t
  :ensure t)

(req-package pyvenv
  :ensure t
  :requires (projectile f)
  :init (progn
          (defvar *python-current-env* ""))
  :config (progn
            (message "Configuring python-env")
            (add-hook 'python-mode-hook
                      (lambda ()
                        (require 'projectile)
                        (let* ((root (projectile-project-root))
                               (env (juiko/python-find-env root)))
                          (when (and env
                                     (not (equal env *python-current-env*)))
                            (progn
                              (setf *python-current-env* env)
                              (pyvenv-activate env)
                              (message "Current python env: %s" *python-current-env*))
                            ))))))

(req-package flycheck
  :ensure t
  :defer t
  :config (progn
            (setq flycheck-perlcritic-severity 5)
            (setq flycheck-ghc-args (list
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

            (custom-set-faces
             '(flycheck-error ((t (:underline "Red1"))))
             '(flycheck-info ((t (:underline "ForestGreen"))))
             '(flycheck-warning ((t (:underline "DarkOrange"))))
             )

            (setq-default flycheck-disabled-checkers
                          '(ruby-rubylint))
            )
  )

(req-package flycheck-package
  :ensure t
  :requires flycheck)

(req-package bind-key
  :ensure t
  :config (eval-after-load "bind-key"
            '(progn
               (bind-key (kbd "M--") 'hippie-expand)
               (bind-key (kbd "M-g M-g")
                         '(lambda ()
                            (interactive)
                            (unwind-protect
                                (progn
                                  (linum-mode t)
                                  (call-interactively 'goto-line))
                              (linum-mode -1)))))))

(req-package iedit
  :ensure t
  :bind (("C-%" . iedit-mode)))

(req-package erc
  :commands (erc erc-tls)
  )

(req-package company
  :ensure t
  :defer 5
  :config (eval-after-load "company"
            '(progn
               (setf company-minimum-prefix-length 2)
               (setf company-show-numbers t)
               (setf company-idle-delay 1)
               (setf company-quickhelp-delay 1)
               (global-company-mode)

               (eval-after-load "cperl-mode"
                 (add-hook 'cperl-mode-hook
                           (lambda ()
                             (make-variable-buffer-local 'company-backends)
                             (setq-local company-backends '((company-gtags
                                                             company-dabbrev
                                                             company-dabbrev-code)))))))))

(defvar *no-smartparens-list*
  '(haskell-mode))

(req-package smartparens
  :ensure t
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

            (add-hook 'prog-mode-hook
                      (lambda ()
                        (unless (-contains? *no-smartparens-list* major-mode)
                          (smartparens-mode))))

            (add-hook 'web-mode-hook
                      (lambda () (require 'smartparens-html)))

            (add-hook 'rust-mode-hook
                      (lambda () (require 'smartparens-rust)))

            (add-hook 'python-mode-hook
                      (lambda () (require 'smartparens-python)))

            (add-hook 'ruby-mode-hook
                      (lambda () (require 'smartparens-ruby)))))

(req-package evil
  :ensure t
  :requires bind-key
  :config (eval-after-load "evil"
            '(progn
               (bind-key "<tab>" 'indent-region evil-visual-state-map)
               (bind-key "C-<tab>" 'indent-whole-buffer evil-normal-state-map)
               (bind-key [return] (lambda ()
                                    (interactive)
                                    (save-excursion
                                      (newline)))
                         evil-normal-state-map)

               (setf evil-move-cursor-back nil)

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
                                      inferior-python-mode
                                      intero-repl-mode
                                      inf-ruby-mode
                                      org-mode
                                      magit-mode)
                        do (evil-set-initial-state mode 'emacs))
               (evil-mode))))

(req-package evil-smartparens
  :ensure t
  :requires (evil smartparens)
  :hook (smartparens-mode . evil-smartparens-mode)
  )

(req-package evil-commentary
  :ensure t
  :requires (evil)
  :config (progn
            (evil-commentary-mode)))

(req-package evil-god-state
  :ensure t
  :commands (evil-execute-in-emacs-state))

(req-package evil-leader
  :ensure t
  :requires evil
  :config (eval-after-load "evil-leader"
            '(progn
               (setf evil-leader/leader (kbd ","))
               (evil-leader/set-key
                 "f" 'counsel-find-file
                 "b" 'switch-to-buffer
                 "g" 'counsel-M-x
                 "k" 'kill-buffer
                 "," 'evil-execute-in-emacs-state
                 ";" 'comment-dwim
                 "e" 'eval-last-sexp
                 "w" 'save-buffer
                 ;; "." 'ggtags-find-tag-dwim
                 "hs" 'swiper
                 "hP"  'counsel-projectile
                 "hpa" 'counsel-projectile-ag
                 "hpg" 'counsel-projectile-grep
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
               (global-evil-leader-mode))))

(req-package web-mode
  :ensure t
  :hook (html-mode . web-mode)
  :mode (("\\.blade\\.php\\'" . web-mode))
  :config (eval-after-load "web-mode"
            '(progn
               (add-hook 'web-mode-hook #'turn-off-smartparens-mode)

               (add-hook 'web-mode-hook
                         (lambda ()
                           (setf web-mode-enable-auto-pairing t)
                           (setf web-mode-enable-css-colorization t)
                           (setf web-mode-enable-block-face t)
                           (setf web-mode-enable-heredoc-fontification t)
                           (setf web-mode-enable-current-element-highlight nil)
                           (setf web-mode-enable-current-column-highlight nil)
                           (setf web-mode-code-indent-offset 2)
                           (setf web-mode-markup-indent-offset 2)
                           (setf web-mode-css-indent-offset 2)))

               (cl-loop
                for extension in '("\\.blade\\.php\\'"
                                   "\\.ejs\\'"
                                   "\\.phtml\\'"
                                   "\\.tpl\\.php\\'"
                                   "\\.[agj]sp\\'"
                                   "\\.as[cp]x\\'"
                                   "\\.erb\\'"
                                   "\\.mustache\\'"
                                   "\\.djhtml\\'"
                                   "\\.html\\'"
                                   "\\.html\\.erb\\'"
                                   "\\html\\.twig\\'"
                                   "\\html\\.jinja\\'"
                                   "\\pdf\\.twig\\'"
                                   "\\.jsx\\'"
                                   "\\.blade\\.php\\'")
                do (add-to-list 'auto-mode-alist `(,extension . web-mode))))))


(req-package js2-mode
  :disabled t
  :ensure t
  :mode "\\.js\\'"
  :config (progn
            (setq js2-basic-offset 4)
            (eval-after-load "js2-mode"
                  '(progn
                     (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))))))

(req-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  )

(req-package color-theme-approximate
  :ensure t

  :if (not window-system)
  :config (eval-after-load "color-theme-approximate"
            '(progn
               (color-theme-approximate-on))))

(req-package cider
  :ensure t
  :hook (clojure-mode . cider-mode)
  )

(req-package counsel-etags
  :ensure t
  :commands (counsel-etags-find-tag-at-point)
  :requires (evil ivy)
  :config (eval-after-load "counsel-etags"
            '(progn
               (evil-define-key 'evil-emacs-state prog-mode-map (kbd "M-.") #'counsel-etags-find-tag-at-point))))

(req-package ivy
  :ensure t
  :commands (counsel-M-x counsel-find-file counsel-describe-function ivy-switch-buffer)
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

(req-package ggtags
  :ensure t
  :defer 5)

(req-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

(req-package ox-twbs
  :ensure t
  :ensure t
  :requires (org))

(req-package evil-lisp-state
  :ensure t
  :require evil evil-leader bind-key
  :init (progn
          (setf evil-lisp-state-global t)
          (setf evil-lisp-state-enter-lisp-state-on-command nil))

  :config (eval-after-load "evil-lisp-state"
            '(progn
               (bind-key "L" 'evil-lisp-state evil-normal-state-map))))

(req-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-init))

(req-package smart-jump
  :ensure t
  :bind (:map evil-normal-state-map
              ("M-." . smart-jump-go)
              ("M-," . smart-jump-back))
  :config (progn
            (smart-jump-setup-default-registers)))

(req-package lsp-mode
  :ensure t
  :commands lsp
  :config (progn
            (require 'lsp-clients)
            (add-hook 'typescript-mode-hook #'lsp)
            (add-hook 'python-mode-hook #'lsp)
            (add-hook 'ruby-mode-hook #'lsp)
            (add-hook 'rust-mode-hook #'lsp)
            (add-hook 'js2-mode-hook #'lsp)
            (add-hook 'js-mode-hook #'lsp)
            (add-hook 'elixir-mode-hook #'lsp)

            (setq lsp-prefer-flymake nil)
            (setq lsp-enable-snippet nil)))

(req-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config (progn
            (add-hook 'prog-mode-hook 'flycheck-mode)))

(req-package company-lsp
  :ensure t
  :commands company-lsp)

(req-package company-lsp
  :ensure t
  :requires lsp-mode
  :config (progn
            (push 'company-lsp company-backends)))

(req-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :config (progn
            (setq-default typescript-indent-level 2)))

(req-package elixir-mode
  :mode "\\.ex\\'"
  :ensure t)

(req-package lsp-elixir
  :disabled t
  :mode "\\.ex\\'"
  :ensure t)

(req-package irony
  :ensure t
  :mode "\\.cpp\\'"
  :config (progn
            (message "Configuring irony-mode for platformio-mode")
            (add-hook 'platformio-mode-hook 'irony-mode)
            (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

(req-package irony-eldoc
  :ensure t
  :requires irony
  :config (progn
            (add-hook 'irony-mode-hook 'irony-eldoc)))

(req-package flycheck-irony
  :ensure t
  :requires irony
  :config (progn
            (add-hook 'irony-mode-hook
                      (lambda ()
                        (message "flycheck-irony: Disabling other c++ checkers")
                        (add-to-list 'flycheck-disabled-checkers 'c/c++-cppcheck)
                        (add-to-list 'flycheck-disabled-checkers 'c/c++-gcc)
                        (add-to-list 'flycheck-disabled-checkers 'c/c++-clang)

                        (message "flycheck-irony: Configuring irony checker")
                        (flycheck-irony-setup)
                        (flycheck-mode)))))

(req-package company-irony
  :ensure t
  :requires irony
  :config (progn
            (add-hook 'irony-mode-hook
                      (lambda ()
                        (message "company-irony: using company-irony as only backend")
                        (setq-local company-backends
                                    '(company-irony))))))

(req-package platformio-mode
  :ensure t
  :requires irony
  :config (progn
            (add-hook 'c++-mode-hook
                      (lambda ()
                        (platformio-mode)
                        (platformio-init-update-workspace)))))

(req-package js2-mode
  :ensure t
  :config (progn
            (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
            (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))
            (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))))

(req-package haskell-mode
  :ensure t
  :mode "\\.hs\\'")

(req-package slime
  :ensure t
  :config (progn
            ))


(req-package lua-mode
  :ensure t)

(req-package-finish)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(kooten-theme dakrone-theme json-mode doom-themes solarized-theme one-themes lua-mode slime haskell-mode js2-mode platformio-mode company-irony flycheck-irony irony-eldoc irony elixir-mode typescript-mode company-lsp lsp-ui lsp-mode smart-jump doom-modeline ox-twbs yaml-mode ggtags counsel-projectile counsel-etags cider color-theme-approximate rust-mode web-mode evil-leader evil-god-state evil-commentary evil-smartparens evil smartparens company iedit flycheck-package flycheck pyvenv magit req-package exec-path-from-shell el-get dracula-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error ((t (:underline "Red1"))))
 '(flycheck-info ((t (:underline "ForestGreen"))))
 '(flycheck-warning ((t (:underline "DarkOrange")))))
