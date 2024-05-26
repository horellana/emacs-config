;; -*- lexical-binding: t;-*-

(message "Set gc-cons-threshold to %s" gc-cons-threshold)
(setq gc-cons-threshold 100000000)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(require 'package)

(progn
  (setq package-archives
        '(("melpa" . "https://melpa.org/packages/")
          ("gnu-devel" . "https://elpa.gnu.org/devel/")
          ("org" . "https://orgmode.org/elpa/"))))

(when (or (not (package-installed-p 'use-package)))
  (package-refresh-contents)
  (package-install 'el-get)
  (package-install 'use-package)
  (package-install 'f))

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))

(require 'quelpa-use-package)
(require 'use-package)

(eval-after-load "flymake"
  '(progn
     (message "loading flymake config")
     (setq flymake-no-changes-timeout 1)))

(eval-after-load "tramp"
  '(progn
     (message "Loading tramp config")
     (setq tramp-default-method "ssh")))

(eval-after-load "eldoc"
  '(progn
     (use-package eldoc-box
       :ensure t
       :config (progn
                 (add-hook 'prog-mode-hook
                           (lambda ()
                             (eldoc-box-hover-mode t)))))

     (setq eldoc-echo-area-prefer-doc-buffer 'maybe)
     (setq eldoc-echo-area-use-multiline-p 1)

     (message "Loaded eldoc config")))

(eval-after-load "go-ts-mode"
  '(progn
     (defun eglot-gopls-before-save-hook ()
       (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
       (lambda ()
         (call-interactively 'eglot-code-action-organize-imports)))

     (setq go-ts-mode-indent-offset 2)

     (add-hook 'before-save-hook #'eglot-format-buffer-on-save)))

(message "Loading emacs config")

(define-advice load-theme (:before (&rest _args) theme-dont-propagate)
  "Discard all themes before loading new."
  (mapc #'disable-theme custom-enabled-themes))

(define-advice load-theme (:after (&rest _args) theme-hide-fringe)
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default)))

(add-hook 'after-save-hook 'whitespace-cleanup)
(set-frame-font "Hack-12")

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq read-process-output-max (* 10 1024 1024))

(setq eldoc-idle-delay 0.75)
(setq flymake-no-changes-timeout 0.5)

(setq line-number-mode t)
(setq column-number-mode t)
(size-indication-mode 1)

(add-hook 'prog-mode-hook 'flymake-mode)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq make-backup-files nil)
(add-to-list 'global-mode-string '(" %i"))

(setq package-native-compile t)

(blink-cursor-mode -1)

(when (not (eq system-type 'darwin))
  (menu-bar-mode -1))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(show-paren-mode 1)

(setf tab-width 2)
(setf tramp-default-method "ssh")
(setf indent-tabs-mode nil)
(setf inhibit-startup-message t)
(setf browse-url-generic-program "firefox-developer-edition")
(setf browse-url-browser-function 'browse-url-firefox)
(setf ring-bell-function 'ignore)
(setf emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq create-lockfiles nil)
(setf backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist '(("~/.emacs.d/saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 1
      kept-old-versions 2
      version-control t)
(setq native-comp-async-jobs-number 12)

(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'prog-mode-hook 'outline-minor-mode)
(add-hook 'prog-mode-hook 'electric-indent-local-mode)

(add-hook 'after-save-hook
          (lambda ()
            (let ((init-file (expand-file-name "~/.emacs.d/init.el")))
              (when (equal (buffer-file-name) init-file)
                (byte-compile-file init-file)))))

(set-face-attribute 'fringe nil
                    :foreground (face-foreground 'default)
                    :background (face-background 'default))

(fset 'yes-or-no-p 'y-or-n-p)

(use-package bind-key
  :ensure t
  :bind (("M--" . hippie-expand)))

(use-package no-littering
  :ensure t
  :config (progn
            (let ((config-dir (expand-file-name "config/" user-emacs-directory))
                  (data-dir (expand-file-name "data/" user-emacs-directory))
                  (auto-save-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

              (progn
                (setq no-littering-etc-directory config-dir)
                (setq no-littering-var-directory data-dir)

                (setq auto-save-file-name-transforms auto-save-transforms)))))

(use-package which-key
  :ensure t
  :config (progn
            (which-key-mode)))

(use-package gcmh
  :ensure t
  :config (progn
            (gcmh-mode 1)))

(use-package vertico
  :ensure t
  :config (progn
            (vertico-mode)))

;; Example configuration for Consult
(use-package consult
  :ensure t
  :bind (;; C-c bindings (mode-specific-map)
         ("M-x" . execute-extended-command)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . switch-to-buffer)                ;; orig. switch-to-buffer
         ;; ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ("C-s" . consult-line)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key "M-.")

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

(use-package embark
  :ensure t
  :after (marginalia)

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-project-extra
  :ensure t
  :after (consult)
  :bind
  (("C-c p f" . consult-project-extra-find)
   ("C-c p o" . consult-project-extra-find-other-window)))


;; Optionally use the `orderless' completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package consult-project-extra
  :after (consult)
  :ensure t
  :bind (("C-c p f" . consult-project-extra-find)
         ("C-c p o" . consult-project-extra-find-other-window)))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package marginalia
  :after (corfu)
  :ensure t)

(use-package corfu
  :ensure t
  :defer t
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  (corfu-scroll-margin 5)        ;; Use scroll margin
  (corfu-auto-delay 1.0)
  :init (progn
          (global-corfu-mode)))

;; Add extensions
(use-package cape
  :ensure t
  ;; Bind dedicated completion commands
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-dict)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-line))

(use-package kind-icon
  :ensure t
  :after (corfu)
  :custom (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config (progn
            (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

(use-package evil
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
                                      magit-mode)
                        do (evil-set-initial-state mode 'emacs))

               (evil-mode))))

(use-package evil-commentary
  :ensure t
  :after (evil)
  :hook (prog-mode . evil-commentary-mode))

(use-package evil-god-state
  :ensure t
  :after (evil)
  :commands (evil-execute-in-emacs-state))

(use-package evil-leader
  :ensure t
  :after (evil)
  :config (progn
            (setf evil-leader/leader (kbd ","))
            (evil-leader/set-key
              "k" 'kill-buffer
              "," 'evil-execute-in-emacs-state
              ";" 'comment-dwim
              "e" 'eval-last-sexp
              "w" 'save-buffer)

            (evil-leader/set-key-for-mode 'emacs-lisp-mode "ma" 'pp-macroexpand-last-sexp)
            (global-evil-leader-mode)))

(use-package evil-lisp-state
  :ensure t
  :after (evil)
  :bind (:map evil-normal-state-map ("L" . evil-lisp-state))
  :config (progn
            (setf evil-lisp-state-global t)
            (setf evil-lisp-state-enter-lisp-state-on-command nil)))

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :config (progn
            (eval-after-load "evil-lisp-state"
              '(progn
                 (evil-leader/set-key-for-mode 'haskell-mode "H" 'haskell-hoogle)))))

(use-package project
  :commands (project-switch-project
             project-find-file))

(use-package platformio-mode
  :ensure t
  :hook (c++-mode-hook . platformio-conditionally-enable))

(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

(use-package evil-org
  :ensure t
  :after (evil)
  :hook (org-mode . evil-org-mode)
  :bind (("C-C a" . org-agenda))
  :custom
  (org-agenda-files (list "~/Documents/org-mode" "~/trabajo/todo.org" "~/Documents/org-mode/journal"))
  (org-log-done 'time)

  :config (progn
            (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
            (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))))

(use-package evil-org-agenda
  :hook (org-agenda-mode . evil-org-agenda-set-keys)
  :bind (("C-C a" . org-agenda)))

(use-package org-journal
  :ensure t
  :commands (org-journal-new-entry)
  :custom
  (org-journal-file-format "%Y%m%d.org")
  (org-journal-dir "~/Documents/org-mode/journal"))

(use-package cider
  :ensure t
  :hook (clojure-mode . cider-mode)
  :config (eval-after-load "evil-lisp-state"
            '(progn
               (evil-leader/set-key-for-mode 'cider-mode "e" 'cider-eval-last-sexp))))

(eval-after-load "evil-leader"
  '(progn
     (evil-leader/set-key-for-mode 'emacs-lisp-mode "ma" 'pp-macroexpand-last-sexp)))

(use-package sly
  :ensure t
  :mode "\\.lisp\\'"
  :config (progn
            (setq inferior-lisp-program "/bin/sbcl")

            (eval-after-load "evil-leader"
              '(progn
                 (evil-leader/set-key-for-mode 'lisp-interaction-mode "ma" 'pp-macroexpand-last-sexp)
                 (evil-leader/set-key-for-mode 'lisp-mode "cl" 'slime-load-file)
                 (evil-leader/set-key-for-mode 'lisp-mode "e" 'slime-eval-last-expression)
                 (evil-leader/set-key-for-mode 'lisp-mode "me" 'slime-macroexpand-1)
                 (evil-leader/set-key-for-mode 'lisp-mode "ma" 'slime-macroexpand-all)
                 (evil-leader/set-key-for-mode 'lisp-mode "sds" 'slime-disassemble-symbol)
                 (evil-leader/set-key-for-mode 'lisp-mode "sdd" 'slime-disassemble-definition)))

            (add-to-list 'auto-mode-alist '("\\.lisp\\'" . lisp-mode))))

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function))

  :config (progn
            (with-eval-after-load "counsel"
              (setq counsel-describe-function-function #'helpful-callable)
              (setq counsel-describe-variable-function #'helpful-variable))))

(use-package eglot
  :ensure t
  :bind (:map eglot-mode-map
              ("M-." . xref-find-definitions))
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (sh-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure))

  :config (progn
            (setq eglot-sync-connect 0
                  eglot-events-buffer-size '(:size 0 :format full)
                  eglot-ignored-server-capabilities '(:hoverProvider
                                                      :documentHighlightProvider)
                  eglot-autoshutdown t)

            (setq eldoc-echo-area-use-multiline-p nil)

            (add-hook 'js-ts-mode
                      (lambda ()
                        (message "js-indent-level")
                        (setq js-indent-level 2)))

            (add-to-list 'eglot-server-programs
                         `(python-ts-mode
                           . ,(eglot-alternatives
                               '("pylsp" "pyls" ("pyright-langserver" "--stdio") "jedi-language-server"))))

            (add-to-list 'eglot-server-programs
                         `((c-ts-mode c++-ts-mode) . ,(eglot-alternatives '("clangd" "ccls"))))

            (add-to-list 'eglot-server-programs
                         '(go-ts-mode . ("gopls")))

            (add-to-list 'eglot-server-programs
                         '(js-ts-mode . ("typescript-language-server" "--stdio")))

            (add-to-list 'eglot-server-programs
                         '(typescript-ts-mode . ("typescript-language-server" "--stdio")))

            (add-to-list 'eglot-server-programs
                         '(tsx-ts-mode . ("typescript-language-server" "--stdio")))

            (add-to-list 'eglot-server-programs
                         '(web-mode . ("typescript-language-server" "--stdio")))))

(use-package eldoc-box
  :ensure t
  :after (eldoc)
  :bind (("C-c K" . eldoc-box-help-at-point)))

(use-package go-mode
  :ensure t
  :mode ("\\.go..'"))

(use-package all-the-icons
  :ensure t)

(use-package doom-themes
  :ensure t)

(use-package ef-themes
  :ensure t)

(use-package smartparens-config
  :ensure smartparens
  :config (progn
            (smartparens-global-mode)))

(use-package almost-mono-themes
  :ensure t)

(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'"))

(use-package sgml-mode
  :hook (tsx-ts-mode . sgml-electric-tag-pair-mode)
  :config (progn
            (add-hook 'tsx-ts-mode 'sgml-electric-tag-pair-mode)))

(use-package tsx-ts-helper-mode
  :load-path "~/.emacs.d/vendor/tsx-ts-helper-mode/"
  :custom (tsx-ts-helper-mode-auto-close-tags nil)
  :hook (tsx-ts-mode . tsx-ts-helper-mode))

(use-package prisma-mode
  :load-path "~/.emacs.d/vendor/emacs-prisma-mode"
  :mode ("\\.prisma\\'"))

(use-package magit
  :ensure t)

(use-package yasnippet
  :ensure t
  :config (progn
            (yas-global-mode 1)))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package treesit-auto
  :ensure t
  :config (progn
            (global-treesit-auto-mode)))

(use-package kotlin-mode
  :defer t
  :ensure t)

(use-package eglot-booster
  :ensure t
  :quelpa (eglot-booster :fetcher github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config	(eglot-booster-mode))

(add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\.sh\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

(add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-ts-mode))

(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-ts-mode))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(setq js-indent-level 2)

(load-theme 'almost-mono-black t)

(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

;; Set garbage collection threshold to 1GB.
(setq gc-cons-threshold #x40000000)

;; When idle for 15sec run the GC no matter what.
(defvar k-gc-timer
  (run-with-idle-timer 15 t
                       (lambda ()
                         (message "Garbage Collector has run for %.06fsec"
                                  (k-time (garbage-collect))))))

(add-function :after
              after-focus-change-function
              (lambda () (unless (frame-focus-state) (garbage-collect))))

(garbage-collect)

(message "Done loading init.el")
