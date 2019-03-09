; -*- lexical-binding: t -*-

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error ((t (:underline "Red1"))))
 '(flycheck-info ((t (:underline "ForestGreen"))))
 '(flycheck-warning ((t (:underline "DarkOrange")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#373b41" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#c5c8c6"))
 '(beacon-color "#cc6666")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "f41ecd2c34a9347aeec0a187a87f9668fa8efb843b2606b6d5d92a653abe2439" "233bb646e100bda00c0af26afe7ab563ef118b9d685f1ac3ca5387856674285d" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "0e0c37ee89f0213ce31205e9ae8bce1f93c9bcd81b1bcda0233061bb02c357a8" "444238426b59b360fb74f46b521933f126778777c68c67841c31e0a68b0cc920" "086970da368bb95e42fd4ddac3149e84ce5f165e90dfc6ce6baceae30cf581ef" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(fci-rule-color "#373b41")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(hl-sexp-background-color "#efebe9")
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (exec-path-from-shell ivy magit typescript-mode company go-mode rust-mode haskell-mode evil smartparens irony flycheck use-package erc company-lsp lsp-mode lsp-typescript lsp-ui forge smart-jump doom-modeline doom-themes monokai monokai-theme solarized-theme material-theme sass-mode klere-theme dracula-theme rjsx-mode jsx-mode ob-erd ox-gfm flycheck-package org-mind-map zenburn-theme color-theme-sanityinc-tomorrow paradox htmlize ox-twbs yasnippet-snippets yasnippet org-plus-contrib yaml-mode web-mode tide tao-theme smart-mode-line slime-company robe req-package rbenv racer pyvenv projectile-rails php-mode php-eldoc minitest js2-mode irony-eldoc intero iedit hlint-refactor hindent go-eldoc ggtags flycheck-rust flycheck-irony flycheck-elm evil-smartparens evil-magit evil-lisp-state evil-leader evil-god-state evil-commentary elm-mode el-get dumb-jump counsel-projectile counsel-etags company-irony company-go company-anaconda color-theme-approximate cider benchmark-init)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))

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

(if (eq system-type 'darwin)
    (set-frame-font "Monaco-13")
  (set-frame-font "DejaVu Sans Mono-12"))

(eval-after-load "bind-key"
  '(progn
     (bind-key "<f8>" 'compile)))


(require 'req-package)

(use-package benchmark-init
  :ensure t
  :disabled t
  :config (add-hook 'after-init-hook 'benchmark-init/deactivate))

(progn
  (blink-cursor-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode 1)
  (global-hl-line-mode 1)
  (show-paren-mode)
  (add-to-list 'default-frame-alist '(cursor-color . "Gray")))

(use-package exec-path-from-shell
  :ensure t
  :if (eq system-type 'darwin)
  :config (progn
            (when (memq window-system '(mac ns x))
              (exec-path-from-shell-initialize))))

(use-package solarized-theme
  :ensure t
  :config (progn
            (load-theme 'solarized-dark t)))

(use-package doom-themes
  :disabled t
  :ensure t
  :config (progn
            (doom-themes-visual-bell-config)
            (doom-themes-org-config)
            (load-theme 'doom-one-light t)))

(defun juiko/python-find-env (project-root)
  "Find the python project env directory, inside PROJECT-ROOT."
  (require 'projectile)
  (car (-intersection (mapcar (lambda (path) (f-join project-root path))
                              (list "env" ".env"))
                      (f-directories project-root))))

(req-package magit
  :ensure t)

(req-package pyvenv
  :ensure t
  :mode "\\.py\\'"
  :requires (projectile f)
  :init (progn
          (defvar *python-current-env* ""))
  :config (eval-after-load "pyvenv"
            '(progn
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
                               )))))))

(req-package flycheck
  :disabled t
  :ensure t
  :defer 1
  :config (progn
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
                          (smartparens-strict-mode))))

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
  :hook (smartparens-strict-mode . evil-smartparens-mode))

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
  :config (progn
            (smart-jump-setup-default-registers)
            (bind-key "M-." 'smart-jump-go evil-normal-state-map)
            (bind-key "M-," 'smart-jump-back)))


(req-package lsp-mode
  :ensure t
  :config (progn
            (add-hook 'typescript-mode-hook #'lsp)
            (add-hook 'python-mode-hook #'lsp)
            (add-hook 'ruby-mode-hook #'lsp)
            (add-hook 'rust-mode-hook #'lsp)
            (add-hook 'js2-mode-hook #'lsp)

            (setq lsp-prefer-flymake t)))

(req-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config (progn
            ;; (req-package 'lsp-ui)
            (add-hook 'lsp-mode-hook 'lsp-ui-mode)))

(req-package company-lsp
  :ensure t
  :commands company-lsp)


(req-package lsp-typescript
  :disabled t
  :ensure t
  :config (progn
            ))

(req-package company-lsp
  :ensure t
  :requires lsp-mode
  :config (progn
            (push 'company-lsp company-backends)))

(req-package typescript-mode
  :ensure t
  :config (progn
            (setq-default typescript-indent-level 2)))

(req-package-finish)
