; -*- lexical-binding: t -*-

(setq lexical-binding t)

(eval-when-compile
  (require 'cl)
  )

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(setq comp-deferred-compilation nil)

(require 'package)

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

(progn
  (blink-cursor-mode -1)

  (when (not (eq system-type 'darwin))
    (menu-bar-mode -1))

  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode 1)
  ;; (global-hl-line-mode)
  (show-paren-mode 1)
  (eldoc-mode t))

(use-package f
  :ensure t)

(use-package tron-legacy-theme
  :ensure t
  :config (progn
	    (load-theme 'tron-legacy t)))

;; (use-package vs-dark-theme
;;   :ensure t
;;   :config (progn
;; 	    (load-theme 'vs-dark t)))

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
(setf backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist '(("~/.emacs.d/saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(progn
  (defalias 'perl-mode 'cperl-mode)
  (eval-after-load "cperl-mode"
    '(progn
       (setf cperl-electric-parens nil
             cperl-electric-keywords nil
             cperl-electric-lbrace-space nil))))

(set-face-attribute 'fringe nil
                    :foreground (face-foreground 'default)
                    :background (face-background 'default))

(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'doc-view-minor-mode-hook 'auto-revert-mode)

(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'outline-minor-mode)


(add-hook 'after-save-hook
          (lambda ()
            (let ((init-file (expand-file-name "~/.emacs.d/init.el")))
              (when (equal (buffer-file-name) init-file)
                (byte-compile-file init-file)))))

(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun windows-subsystem-linux-p ()
  "Return t if running in WSL."
  (not
   (null (string-match-p "Microsoft"
                         (shell-command-to-string "uname -a")))))

(add-to-list 'display-buffer-alist
             (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

(eval-after-load "js-mode"
  '(progn
     (setq js-indent-level 2)))

(eval-after-load "projectile"
  '(eval-after-load "f"
     '(progn
	      ;; (message "loading gnu global configuration")
	      (message "loading ctags configuration")

        (defvar *horellana/ctags-process-running-p*)

	      (defun horellana/ctags-exists-p (root)
	        (require 'f)
	        (-contains-p  (f-files root)
                        (f-join root "tags")))
        
	      (defun horellana/sync-ctags-create (project-root)
	        (let* ((default-directory project-root)
                 (output-file (format "'%s%s'" project-root "tags"))
		             (cmd "ctags-generate.sh"))

	          (unless *horellana/ctags-process-running-p*
	            (setq-local *horellana/ctags-process-running-p* t)
	            (async-start-process "ctags-generate"
				                           "ctags-generate.sh"
				                           (lambda () (setq-local *horellana/ctags-process-running-p* nil))
				                           output-file))))
        
        (defun horellana/sync-ctags-update (project-root)
	        (message "sync-ctags-update")
	        (horellana/sync-ctags-create project-root))
	      
	      (defun horellana/sync-ctags (root)
	        (if (horellana/ctags-exists-p root)
	            (horellana/sync-ctags-update root)
	          (let ((default-directory root))
	            (horellana/sync-ctags-create root))))
        
	      (defvar *horellana/ctags-modes*)
        
        (setf *horellana/ctags-modes*
	            '(c++-mode))
        
        (add-hook 'after-save-hook
		              (lambda ()
	                  (message "ctags after-save-hook")
		                (require 'projectile)
		                (let ((generate-tags-p (member major-mode *horellana/ctags-modes*))
			                    (project-root (projectile-project-root)))
		                  (when (and generate-tags-p
                                 project-root)
		                    (message "project root: %s" project-root)
                        (horellana/sync-ctags project-root))))))))

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
     (setq org-agenda-files (list "~/emacs-org/todos.org"))
     
     (define-key global-map "\C-Ca" 'org-agenda)
     (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))))

(set-frame-font "UbuntuMono-12")

(eval-after-load "bind-key"
  '(progn
     (bind-key "<f8>" 'compile)))

(require 'req-package)

(use-package benchmark-init
  :disabled t
  :ensure t
  :config (add-hook 'after-init-hook 'benchmark-init/deactivate))

(defun juiko/python-find-env (project-root)
  "Find the python project env directory, inside PROJECT-ROOT."
  (require 'projectile)
  (car (-intersection
	      (mapcar (lambda (path) (f-join project-root path))
		            (list "env" ".env"))
	      (f-directories project-root))))

(req-package magit
  :defer t
  :ensure t)

(req-package pyvenv
  :ensure t
  :disabled t
  :init (progn
          (defvar *python-current-env* ""))
  :config (progn
            (add-hook 'python-mode-hook
                      (lambda ()
                        (message "Configuring python-env")
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
	          
	          (add-hook 'python-mode-hook
		                  (lambda ()
			                  (flycheck-mode)))))

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
               (setf company-minimum-prefix-length 3)
               (setf company-show-numbers t)
               (setf company-idle-delay 0.5)
               (setf company-quickhelp-delay 1)
               
               (global-company-mode)
               
               (add-hook 'prog-mode-hook
                         (lambda ()
		           (if (eq major-mode 'c++-mode)
			       (progn
			         (message "c++-mode company-backends")
			         (setq-local company-backends '((company-gtags company-ctags company-capf))))
                             (setq-local company-backends '(company-capf))))))))

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
	             (custom-set-variables
	              '(evil-undo-system 'undo-redo))


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
  :config (progn
            (setf evil-leader/leader (kbd ","))
            (evil-leader/set-key
              "f" 'counsel-find-file
              "b" 'ivy-switch-buffer
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
            (global-evil-leader-mode)))

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


(req-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  )

(req-package color-theme-approximate
  :ensure t
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

(req-package company-ctags
  :ensure t)
;; :config (progn
;;           (eval-after-load "company"
;;             '(progn
;;                (add-hook 'c++-mode-hook
;;                          (lambda ()
;;                            (setq-local company-backends '((company-capf company-dabbrev-code company-ctags)))))))))

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

(req-package mood-line
  :ensure t
  :config (progn
	          (mood-line-mode)))


(req-package smart-jump
  :ensure t
  :bind (:map evil-normal-state-map
              ("M-." . smart-jump-go)
              ("M-," . smart-jump-back))
  :config (progn
            (smart-jump-setup-default-registers)))

(req-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :config (progn
            (setq-default typescript-indent-level 2)))

(req-package elixir-mode
  :mode "\\.ex\\'"
  :ensure t)

(defun horellana/platformio-configure-include-paths ()
  (let* ((cmd (format "find %s%s"
                      (projectile-project-root)
                      ".pio/libdeps/nano -maxdepth 1 -not -iname '*nano'"))

         (paths (split-string (shell-command-to-string cmd)))
         (ld-library-path (string-join paths "/:")))

    (message ld-library-path)
    (setenv "LD_LIBRARY_PATH" ld-library-path)))

(req-package platformio-mode
  :ensure t
  :requires irony
  :defer t
  :config (progn
	          (add-hook 'c++-mode-hook
		                  (lambda ()
			                  (platformio-conditionally-enable)))))

(req-package haskell-mode
  :ensure t
  :mode "\\.hs\\'")

(req-package slime
  :ensure t
  :defer t
  :config (progn
            ))

(req-package feebleline
  :ensure t
  :defer t
  :config (progn
            (feebleline-mode 1)))

(req-package company-jedi
  :ensure t
  :defer t
  :config (progn
            (add-hook 'python-mode-hook
	                    (lambda ()
                        (setq-local company-backends '(company-jedi))))))


(req-package treemacs
  :ensure t
  :defer t
  :config (progn
            (with-eval-after-load 'winum
              (define-key winum-keymap (kbd "M-0") #'treemacs-select-window)) ))

(req-package treemacs-evil
  :ensure t
  :defer t
  :require (treemacs evil))

(req-package treemacs-projectile
  :ensure t
  :defer t
  :require (treemacs projectile))

(req-package no-littering
  :ensure t
  :defer t
  :config (progn
	          (setq auto-save-file-name-transforms
		              `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))))

(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

(setq create-lockfiles nil)

(req-package wakatime-mode
  :ensure t
  :defer t
  :config (progn
	          (global-wakatime-mode)))

(req-package org-journal
  :defer t
  :ensure t)

(req-package ccls
  :ensure t)

(req-package prescient
  :ensure t)

(req-package ivy-prescient
  :ensure t
  :config (progn
            (ivy-prescient-mode)))

(req-package company-prescient
  :ensure t
  :config (progn
            (company-prescient-mode)))

(req-package pipenv
  :ensure t
  :hook (python-mode . pipenv-mode)
  :config (progn
	          (defun pipenv-shell ()
	            (message "(pipenv-shell) STUB"))
	          (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended)))

(req-package yasnippet
  :ensure t)

(req-package company-ctags
  :ensure t)

(req-package async
  :ensure t)

(req-package-finish)

(add-hook 'python-mode-hook
	        (lambda ()
	          (pipenv-mode)
	          (pipenv-activate)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2D2A2E" "#CC6666" "#A9DC76" "#FFD866" "#78DCE8" "#FF6188" "#78DCE8" "#FCFCFA"])
 '(custom-safe-themes
   '("c5692610c00c749e3cbcea09d61f3ed5dac7a01e0a340f0ec07f35061a716436" "2422e84e81ce5ff243b9b8dd4076b8bab9b5c630c9b8a7533ec3c5b3fed23329" "1fbd63256477789327fe429bd318fb90a8a42e5f2756dd1a94805fc810ae1b62" "912cac216b96560654f4f15a3a4d8ba47d9c604cbc3b04801e465fb67a0234f0" "79bc32a7c8da2ca2dd33591df9485258293e0e6e03d0ff4a2403a6882dcfdb2b" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "82360e5f96244ce8cc6e765eeebe7788c2c5f3aeb96c1a765629c5c7937c0b5b" "6c3b5f4391572c4176908bb30eddc1718344b8eaff50e162e36f271f6de015ca" "0cb1b0ea66b145ad9b9e34c850ea8e842c4c4c83abe04e37455a1ef4cc5b8791" "5d09b4ad5649fea40249dd937eaaa8f8a229db1cec9a1a0ef0de3ccf63523014" "7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" "54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" "2f1518e906a8b60fac943d02ad415f1d8b3933a5a7f75e307e6e9a26ef5bf570" "b5fff23b86b3fd2dd2cc86aa3b27ee91513adaefeaa75adc8af35a45ffb6c499" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "b66970f42d765a40fdb2b6b86dd2ab6289bed518cf4d8973919e5f24f0ca537b" "fe94e2e42ccaa9714dd0f83a5aa1efeef819e22c5774115a9984293af609fce7" "9b01a258b57067426cc3c8155330b0381ae0d8dd41d5345b5eddac69f40d409b" "3577ee091e1d318c49889574a31175970472f6f182a9789f1a3e9e4513641d86" "d71aabbbd692b54b6263bfe016607f93553ea214bc1435d17de98894a5c3a086" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "3d3807f1070bb91a68d6638a708ee09e63c0825ad21809c87138e676a60bda5d" "a92e9da0fab90cbec4af4a2035602208cebf3d071ea547157b2bfc5d9bd4d48d" "e2acbf379aa541e07373395b977a99c878c30f20c3761aac23e9223345526bcc" "632694fd8a835e85bcc8b7bb5c1df1a0164689bc6009864faed38a9142b97057" "9b272154fb77a926f52f2756ed5872877ad8d73d018a426d44c6083d1ed972b1" "34b3219ae11acd81b2bb7f3f360505019f17d7a486deb8bb9c1b6d13c6616d2e" "6177ecbffb8f37756012c9ee9fd73fc043520836d254397566e37c6204118852" "2a749c20af891c16571527d07976bbcf2bf31819fa7d322942b73386019f4d58" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "71e5acf6053215f553036482f3340a5445aee364fb2e292c70d9175fb0cc8af7" "5b809c3eae60da2af8a8cfba4e9e04b4d608cb49584cb5998f6e4a1c87c057c4" "dcdd1471fde79899ae47152d090e3551b889edf4b46f00df36d653adc2bf550d" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "361f5a2bc2a7d7387b442b2570b0ef35198442b38c2812bf3c70e1e091771d1a" "f2b56244ecc6f4b952b2bcb1d7e517f1f4272876a8c873b378f5cf68e904bd59" "dde8c620311ea241c0b490af8e6f570fdd3b941d7bc209e55cd87884eb733b0e" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "7f791f743870983b9bb90c8285e1e0ba1bf1ea6e9c9a02c60335899ba20f3c94" "d5f8099d98174116cba9912fe2a0c3196a7cd405d12fa6b9375c55fc510988b5" "7c4cfa4eb784539d6e09ecc118428cd8125d6aa3053d8e8413f31a7293d43169" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" "e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "58c3313b4811ed8b30239b1d04816f74d438bcb72626d68181f294b115b7220d" "51956e440cec75ba7e4cff6c79f4f8c884a50b220e78e5e05145386f5b381f7b" "229c5cf9c9bd4012be621d271320036c69a14758f70e60385e87880b46d60780" "285efd6352377e0e3b68c71ab12c43d2b72072f64d436584f9159a58c4ff545a" "f9cae16fd084c64bf0a9de797ef9caedc9ff4d463dd0288c30a3f89ecf36ca7e" "615123f602c56139c8170c153208406bf467804785007cdc11ba73d18c3a248b" "1d50bd38eed63d8de5fcfce37c4bb2f660a02d3dff9cbfd807a309db671ff1af" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "fa3bdd59ea708164e7821574822ab82a3c51e262d419df941f26d64d015c90ee" "cb96a06ed8f47b07c014e8637bd0fd0e6c555364171504680ac41930cfe5e11e" "cbd014af7a08121d1d30388c519b7374496351743bbd59371c44786640056623" "bc836bf29eab22d7e5b4c142d201bcce351806b7c1f94955ccafab8ce5b20208" default))
 '(evil-undo-system 'undo-redo)
 '(fci-rule-color "#4C4A4D" t)
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(jdee-db-active-breakpoint-face-colors (cons "#19181A" "#FCFCFA"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#19181A" "#A9DC76"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#19181A" "#727072"))
 '(objed-cursor-color "#CC6666")
 '(package-selected-packages
   '(tron-legacy-theme melancholy-theme melacholy-theme vs-dark-theme emacs-async emacs-asyn company-ctags cyberpunk minsk-theme leuven-theme yasnippet pipenv company-prescient ivy-prescient prescient mood-line minimal-theme ccls wakatime-mode no-littering org-journal arc-dark-theme company-box treemacs-evil treemacs-projectile treemacs spacemacs spacemacs-them spacemacs-theme vscode-dark-plus-theme dracula-theme badger-theme moe-theme tao-theme doom-theme feebleline cyberpunk-theme doom-themes exec-path-from-shell slime haskell-mode platformio-mode company-irony flycheck-irony irony-eldoc irony elixir-mode typescript-mode smart-jump doom-modeline ox-twbs yaml-mode ggtags counsel-projectile counsel-etags cider color-theme-approximate rust-mode js2-mode web-mode evil-lisp-state evil-leader evil-god-state evil-commentary evil-smartparens evil smartparens company iedit flycheck-package flycheck pyvenv magit req-package use-package el-get))
 '(pdf-view-midnight-colors (cons "#FCFCFA" "#2D2A2E"))
 '(rustic-ansi-faces
   ["#2D2A2E" "#CC6666" "#A9DC76" "#FFD866" "#78DCE8" "#FF6188" "#78DCE8" "#FCFCFA"])
 '(vc-annotate-background "#2D2A2E")
 '(vc-annotate-color-map
   (list
    (cons 20 "#A9DC76")
    (cons 40 "#c5da70")
    (cons 60 "#e2d96b")
    (cons 80 "#FFD866")
    (cons 100 "#fec266")
    (cons 120 "#fdad66")
    (cons 140 "#FC9867")
    (cons 160 "#fd8572")
    (cons 180 "#fe737d")
    (cons 200 "#FF6188")
    (cons 220 "#ee627c")
    (cons 240 "#dd6471")
    (cons 260 "#CC6666")
    (cons 280 "#b56869")
    (cons 300 "#9f6b6c")
    (cons 320 "#886d6f")
    (cons 340 "#4C4A4D")
    (cons 360 "#4C4A4D")))
 '(vc-annotate-very-old-color nil)
 '(wakatime-cli-path "/usr/bin/wakatime")
 '(wakatime-python-bin nil)
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error ((t (:underline "Red1"))))
 '(flycheck-info ((t (:underline "ForestGreen"))))
 '(flycheck-warning ((t (:underline "DarkOrange")))))


(message "Done loading init.el")
