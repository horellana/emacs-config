;; -*- lexical-binding: t -*-

;; (require 'benchmark-init)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(package-selected-packages
   '(counsel-projectile benchmark-init moe-theme darkokai-theme monokai-alt-theme monokai-theme auctex hy-mode yaml-mode web-mode tide smart-mode-line slime-company robe req-package rbenv racer pyvenv projectile-rails php-mode php-eldoc minitest js2-mode irony-eldoc intero iedit hlint-refactor hindent helm-swoop helm-projectile helm-gtags helm-ag go-eldoc flycheck-rust flycheck-irony flycheck-elm evil-smartparens evil-magit evil-lisp-state evil-leader evil-god-state evil-commentary elm-mode el-get dumb-jump counsel-etags company-irony company-go company-anaconda cider)))


(setq lexical-binding t)
(setq package-menu-async nil)

(eval-when-compile
  (require 'cl))


(require 'package)
;; (package-initialize)

(progn
  (setq package-archives
  '(("melpa" . "https://melpa.org/packages/")
    ("gnu" . "https://elpa.gnu.org/packages/"))))
(when (or (not (package-installed-p 'use-package))
    (not (package-installed-p 'req-package)))
  (package-refresh-contents)
  (package-install 'el-get)
  (package-install 'use-package)
  (package-install 'req-package))

(require 'use-package)
(require 'req-package)


(use-package benchmark-init
  :disabled t
  :config (eval-after-load "benchmark-init"
            '(progn
               (add-hook 'after-init-hook 'benchmark-init/deactivate))))


(req-package pyvenv
  :mode "\\.py\\'"
  :require projectile f
  :init (progn
          (defun juiko/python-find-env (project-root)
            "Find the python project env directory, inside PROJECT-ROOT."
            (car (-intersection (mapcar (lambda (path) (f-join project-root path))
                                        (list "env" ".env"))
                                (f-directories project-root))))

          (defvar *python-current-env* ""))
  :config (eval-after-load "pyvenv"
            '(progn
               (add-hook 'python-mode-hook
                         (lambda ()
                           (let* ((root (projectile-project-root))
                                  (env (juiko/python-find-env root)))

                             (when (and env
                                        (not (equal env *python-current-env*)))
                               (progn
                                 (setf *python-current-env* env)
                                 (pyvenv-activate env)
                                 (message "Current python env: %s" *python-current-env*))
                               )))))))

(req-package irony
  :disabled t
  :config (eval-after-load "irony"
      '(progn
         (add-hook 'c-mode-hook 'irony-mode)
         (add-hook 'c++-mode-hook 'irony-mode)
         (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))))

(req-package company-irony
  :require irony
  :config (eval-after-load "company-irony"
      '(progn
         (add-hook 'irony-mode-hook
       (lambda ()
         (setq-local company-backends '(company-irony)))))))

(req-package flycheck-irony
  :require irony
  :config (eval-after-load "flycheck-irony"
      '(progn
         (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))))

(req-package irony-eldoc
  :require irony
  :config (eval-after-load "irony-eldoc"
      '(progn
         (add-hook 'irony-mode-hook 'irony-eldoc))))

(req-package f
  :defer t
  )

(req-package bind-key
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
  :bind (("C-%" . iedit-mode)))

(req-package erc
  :defer t
  :commands (erc erc-tls)
  )

(req-package company
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

(req-package company-quickhelp
  :disabled
  :if window-system
  :require company
  :config (add-hook 'company-mode-hook 'company-quickhelp-mode))

(req-package flycheck
  :defer 5
  :config (eval-after-load "flycheck"
      '(progn
         (global-flycheck-mode)

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

         (add-hook 'ruby-mode-hook
       (flycheck-disable-checker 'ruby-rubylint nil))
         )))

(req-package flycheck-pos-tip
  :disabled t
  :if window-system
  :require flycheck

  :config (eval-after-load "flycheck-pos-tip"
      '(progn
         (setq flycheck-display-errors-function
         #'flycheck-pos-tip-error-messages))))

(defvar *no-smartparens-list*
  '(haskell-mode))

(req-package smartparens

  :config (eval-after-load "smartparens"
      '(progn
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
           (smartparens-strict-mode))))

         (add-hook 'web-mode-hook
                   (lambda () (require 'smartparens-html)))

         (add-hook 'rust-mode-hook
                   (lambda () (require 'smartparens-rust)))

         (add-hook 'python-mode-hook
                   (lambda () (require 'smartparens-python)))

         (add-hook 'ruby-mode-hook
                   (lambda () (require 'smartparens-ruby))))))

(req-package slime-company
  :mode "\\.lisp\\'"
  :require company)

(req-package slime
  :mode "\\.lisp\\'"
  :require slime-company
  :init (progn
    (setf inferior-lisp-program "sbcl")
    (setf slime-contrib '(slime-fancy slime-company))
    (setf slime-sbcl-manual-root "/usr/local/share/info/sbcl.info")
    (add-hook 'lisp-mode-hook
        (lambda ()
          (unless (slime-connected-p)
      (save-excursion (slime))))))

  :config (eval-after-load "slime"
      '(progn
         (slime-setup '(slime-fancy slime-company))
         (cl-loop for hook in '(slime-mode-hook slime-repl-mode-hook)
      do
      (add-hook hook
          (lambda ()
            (setq-local company-backends '(company-slime))))))))

(req-package evil
  :require bind-key
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
              magit-mode)
      do (evil-set-initial-state mode 'emacs))


         (evil-mode))))

(req-package evil-lisp-state

  :require evil evil-leader bind-key
  :init (progn
    (setf evil-lisp-state-global t)
    (setf evil-lisp-state-enter-lisp-state-on-command nil))

  :config (eval-after-load "evil-lisp-state"
      '(progn
         (bind-key "L" 'evil-lisp-state evil-normal-state-map)
         )))

(req-package evil-smartparens

  :require evil smartparens
  :config (eval-after-load "evil-smartparens"
      '(progn
         (add-hook 'smartparens-strict-mode-hook 'evil-smartparens-mode))))

(req-package evil-commentary

  :require evil
  :config (eval-after-load "evil-commentary"
      '(progn
         (evil-commentary-mode)
         )))

(req-package evil-god-state

  :require evil god-mode
  :config (eval-after-load "evil-god-state"
      '(progn
         (bind-key "ESC" 'evil-normal-state evil-god-state-map))))

(req-package evil-leader
  :require evil
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
     "." 'ggtags-find-tag-dwim
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
         ;; (evil-leader/set-key-for-mode 'projectile-mode (kbd "p")'helm-projectile)
         (global-evil-leader-mode))))

(req-package evil-magit

  :require evil magit
  )

(req-package material-theme
  :if window-system
  :disabled t

  :config (eval-after-load "material-theme"
      '(progn
         (load-theme 'material-light t)

         (add-hook 'after-init-hook
       (lambda ()
         (set-face-attribute 'fringe
                 nil
                 :background "#FAFAFA"
                 :foreground "#FAFAFA"))))))

(req-package leuven-theme
  :disabled t
  :if window-system

  :config (eval-after-load "leuven-theme"
      '(progn
         (add-hook 'after-init-hook
       (lambda ()
         (load-theme 'leuven t)
         (set-face-attribute 'fringe
                 nil
                 :background "2e3436"
                 :foreground "2e3436")
         )))))

(req-package projectile
  :commands (projectile-find-file-dwim
             counsel-projectile
             counsel-projectile-ag
             counsel-projectile-grep)
  :config (eval-after-load "projectile"
      '(progn
         (add-hook 'after-init-hook 'projectile-global-mode))))

(req-package magit
  :commands (magit-status magit-init magit-log magit-diff)
  :init (progn
    (setf magit-last-seen-setup-instructions "1.4.0")))

(req-package haskell-mode
  :mode ("\\.hs\\'")
  :config (eval-after-load "haskell-mode"
      '(progn
         (add-hook 'haskell-mode-hook 'haskell-doc-mode)
         (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
         ;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
         (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
         (add-hook 'haskell-mode-hook (lambda ()
                (electric-indent-local-mode -1)))

         (setf haskell-process-type 'stack-ghci)
         (setf haskell-process-path-ghci "stack")
         (setf haskell-process-args-ghci '("ghci "))

         (setf haskell-process-suggest-remove-import-lines t)
         (setf haskell-process-auto-import-loaded-modules t)
         (setf haskell-process-log nil)
         (setf haskell-stylish-on-save t))))


(req-package intero
  :require haskell-mode
  :config (eval-after-load "intero"
      '(progn
         (add-hook 'haskell-mode-hook 'intero-mode)
         (add-hook 'intero-mode-hook
       (lambda ()
         (progn
           (make-variable-buffer-local 'company-backends)
           (setq-local company-backends '(company-intero))))))))

(req-package hindent
  :mode "\\.hs\\'"
  :require haskell-mode
  :config (eval-after-load "hindent"
      '(progn
         (setf hindent-style "chris-done")
         (evil-define-key 'evil-visual-state hindent-mode-map "TAB"
     'hindent-reformat-region)
         (add-hook 'haskell-mode-hook 'hindent-mode))))

(req-package flycheck-haskell

  :require flycheck haskell-mode
  :disabled t

  :config (eval-after-load "flycheck-haskell"
      '(progn
         (add-hook 'haskell-mode-hook 'flycheck-mode)
         (add-hook 'flycheck-mode-hook 'flycheck-haskell-configure))))

(req-package company-ghci

  :disabled t
  :require company haskell-mode

  :config (eval-after-load "company-ghci"
      '(progn
         (add-hook 'haskell-mode-hook
       (lambda ()
         (setq-local company-backends
               '((company-ghci
            company-dabbrev-code))))))))

(req-package hlint-refactor
  :mode "\\.hs\\'"
  :require haskell-mode
  :config (eval-after-load "hlint-refactor"
      '(progn
         (bind-key "C-c h r" 'hlint-refactor-refactor-at-point hlint-refactor-mode-map)

         (add-hook 'haskell-mode-hook 'hlint-refactor-mode))))

(req-package anaconda-mode
  :mode ("\\.py\\'" . python-mode)
  :config (eval-after-load "anaconda-mode"
      '(progn
         (add-hook 'python-mode-hook 'anaconda-mode)
         (add-hook 'python-mode-hook 'eldoc-mode))))

(req-package company-anaconda
  :mode "\\.py\\'"
  :require company anaconda-mode
  :config (eval-after-load "company-anaconda"
      '(progn
         (add-hook 'anaconda-mode-hook
       (lambda ()
         (make-variable-buffer-local 'company-backends)
         (setq-local company-backends '(company-anaconda)))))))


(req-package web-mode
  :mode "\\.html\\'"
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
           "\\.phtml\\'"
           "\\.tpl\\.php\\'"
           "\\.[agj]sp\\'"
           "\\.as[cp]x\\'"
           "\\.erb\\'"
           "\\.mustache\\'"
           "\\.djhtml\\'"
           "\\.html\\'"
           "\\html\\.twig\\'"
           "\\html\\.jinja\\'"
           "\\pdf\\.twig\\'")
    do (add-to-list 'auto-mode-alist `(,extension . web-mode))))))


(req-package php-mode
  :mode "\\.php\\'"
  :config (eval-after-load "php-mode"
      '(progn
         (require 'php-ext)
         (setf php-template-compatibility nil)
         (setf php-lineup-cascaded-calls t)

         (add-hook 'php-mode-hook
       'php-enable-symfony2-coding-style)

         (add-hook 'php-mode-hook
       (lambda ()
         (setq-local company-backends '((company-gtags
                 company-dabbrev-code)
                ))))
         (with-eval-after-load "yasnippet"
     (add-hook 'php-mode-hook 'yas-minor-mode)))))

(req-package php-eldoc
  :require php-mode
  :config (eval-after-load "php-eldoc"
      '(progn
         (add-hook 'php-mode-hook 'php-eldoc-enable))))

(req-package js2-mode
  :mode "\\.js\\'"
  :config (eval-after-load "js2-mode"
      '(progn
         (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))))

(req-package rust-mode
  :mode "\\.rs\\'"

  )

(req-package racer
  :require rust-mode company
  :mode "\\.rs\\'"
  :config (eval-after-load "racer"
      '(progn
         ;; (setf racer-cmd "/home/juiko/git/racer/target/release/racer")
         (setf racer-rust-src-path (concat
            (replace-regexp-in-string "\n\\'" ""
                    (shell-command-to-string "rustc --print sysroot"))
            "/lib/rustlib/src/rust/src"))
         (add-hook 'rust-mode-hook 'racer-mode)
         (add-hook 'racer-mode-hook 'eldoc-mode)
         (evil-define-key 'evil-insert-state rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)

         (add-hook 'racer-mode-hook
       (lambda ()
         (make-variable-buffer-local 'company-minimum-prefix-length)
         (make-variable-buffer-local 'company-idle-delay)

         (setq-local company-minimum-prefix-length 3)
         (setq-local company-idle-delay 2)
         )))))

(req-package flycheck-rust
  :mode "\\.rs\\'"
  :require rust-mode flycheck
  :config (eval-after-load "flycheck-rust"
      '(progn
         (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))))

(req-package elm-mode
  :mode "\\.elm\\'"
  )

(req-package flycheck-elm
  :mode "\\.elm\\'"
  :require elm-mode flycheck

  :config (eval-after-load "flycheck-elm"
      '(progn
         (add-hook 'flycheck-mode-hook 'flycheck-elm-setup))))

(req-package color-theme-approximate

  :if (not window-system)
  :config (eval-after-load "color-theme-approximate"
      '(progn
         (color-theme-approximate-on))))


(req-package robe
  :mode "\\.rb\\'"
  :config (eval-after-load "robe"
      '(progn
         (add-hook 'ruby-mode-hook 'robe-mode)
         (add-hook 'robe-mode-hook
       (lambda ()
         (make-variable-buffer-local 'company-backends)
         (setq-local company-backends '(company-robe
                company-dabbrev-code))))
         (add-hook 'robe-mode-hook 'eldoc-mode))))

(req-package rbenv

  :require robe
  :diminish global-rbenv-mode
  :config (eval-after-load "rbenv"
      '(progn
         (global-rbenv-mode)
         )))


(req-package projectile-rails
  :mode "\\.rb\\'"
  :require projectile
  :config (eval-after-load "projectile-rails"
      '(progn
         (add-hook 'projectile-mode-hook 'projectile-rails-on))))

(req-package minitest
  :mode "\\.rb\\'"
  :config (eval-after-load "minitest"
      '(progn
         (add-hook 'ruby-mode-hook 'minitest-mode))))

(req-package cider
  :mode ("\\.clj\\'" "\\.cljs\\'")
  :config (eval-after-load "cider"
      '(progn
         (add-hook 'clojure-mode-hook 'cider-mode))))

(req-package tide
  :mode "\\.ts\\'"

  :config (eval-after-load "tide"
            '(progn
               (add-hook 'typescript-mode-hook  'tide-setup)
               )))

(req-package dumb-jump
  :commands (dumb-jump-go dumb-jump-quick-look dumb-jump-go-prompt)
  :config (eval-after-load "dumb-jump"
      '(progn
         (dumb-jump-mode))))

(req-package go-mode
  :mode "\\.go\\'"
  :config (eval-after-load "go-mode"
      '(progn
         (add-hook 'before-save-hook #'gofmt-before-save))))

(req-package go-eldoc
  :require go-mode
  :config (eval-after-load "go-eldoc"
      '(progn
         (add-hook go-mode-hook 'eldoc-mode))))

(req-package company-go
  :requires go-mode company
  :config (eval-after-load "company-go"
      '(progn
         (add-hook go-mode-hook
       (lambda ()
         (make-variable-buffer-local 'company-backends)
         (setq-local company-backends '(company-go)))))))

(req-package counsel-etags
  :commands (counsel-etags-find-tag-at-point)
  :requires evil ivy
  :config (eval-after-load "counsel-etags"
      '(progn
         (evil-define-key 'evil-emacs-state prog-mode-map (kbd "M-.") #'counsel-etags-find-tag-at-point))))

(req-package smart-mode-line
  :config (eval-after-load "smart-mode-line"
      '(progn
         (setq sml/theme 'dark)
         (sml/setup))))


(req-package ivy
  :defer 5
  :commands (counsel-M-x counsel-find-file counsel-describe-function)
  :config (eval-after-load "ivy"
            '(progn
               (ivy-mode 1)
               (counsel-mode 1))))

(req-package counsel-projectile
  :defer 5
  :require ivy projectile
  :commands (counsel-projectile counsel-projectile-ag counsel-projectile-grep)
  :config (eval-after-load "counsel-projectile"
            '(progn
               (counsel-projectile-mode))))

(req-package-finish)


(use-package monokai-theme
  :config (eval-after-load "monokai"
      '(progn
         (load-theme 'monokai t))))

(add-hook 'before-save-hook 'whitespace-cleanup)

(add-hook 'doc-view-minor-mode-hook 'auto-revert-mode)

;;; Windows shut the fuck up,mgs
(setq ring-bell-function 'ignore)

(defun windows-subsystem-linux-p ()
  "Return t if running in WSL."
  (not
   (null (string-match-p "Microsoft"
       (shell-command-to-string "uname -a")))))

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


(eval-after-load "python-mode"
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

(defun gtags-exists-p (root)
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

(setf *gtags-modes*
      '(web-mode
  php-mode
  cperl-mode
  ruby-mode
  ))


(add-hook 'after-save-hook
    (lambda ()
      (let ((generate-tags-p (member major-mode *gtags-modes*))
      (project-root (projectile-project-root)))

        (when (and generate-tags-p
       project-root)
    (async-gtags project-root)))))

(defun endless/upgrade ()
  "Upgrade all packages, no questions asked."
  (interactive)
  (save-window-excursion
    (list-packages)
    (package-menu-mark-upgrades)
    (package-menu-execute 'no-query)))

(add-hook 'prog-mode-hook 'eldoc-mode)

(setq-default inhibit-startup-message t)
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default backup-by-copying t      ; don't clobber symlinks
        backup-directory-alist '(("." . "~/.emacs.d/saves"))    ; don't litter my fs tree
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)

(add-hook 'after-save-hook
    (lambda ()
      (let ((init-file (expand-file-name "~/.emacs.d/init.el")))
        (when (equal (buffer-file-name) init-file)
    (byte-compile-file init-file)))))

(progn
  (defalias 'perl-mode 'cperl-mode)
  (setq-default cperl-electric-parens nil
    cperl-electric-keywords nil
    cperl-electric-lbrace-space nil))

(setf backup-directory-alist
      '((".*" . "/home/juiko/.emacs.d/cache/"))
      auto-save-file-name-transforms
      '((".*" "/home/juiko/.emacs.d/cache/" t))
      auto-save-list-file-prefix
      "/home/juiko/.emacs.d/cache/")


(setq-default tab-width 2)
(setq-default tramp-default-method "ssh")
(setq-default indent-tabs-mode nil)

(juiko/look-config)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error ((t (:underline "Red1"))))
 '(flycheck-info ((t (:underline "ForestGreen"))))
 '(flycheck-warning ((t (:underline "DarkOrange")))))
