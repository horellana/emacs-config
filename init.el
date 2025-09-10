;;; -*- lexical-binding: t; -*-

;;; Startup Performance Tweaks
(setq gc-cons-threshold 100000000) ; 100 MB para evitar pausas frecuentes
(setq read-process-output-max (* 10 1024 1024))
(setq native-comp-async-jobs-number 12)

;;; User Interface Tweaks
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(show-paren-mode 1)
(global-hl-line-mode 1)
(setq inhibit-startup-message t)
(set-frame-font "Menlo-18" nil t)
(set-fringe-mode 0)
(set-face-attribute 'fringe nil :foreground (face-foreground 'default) :background (face-background 'default))

(setq frame-resize-pixelwise t)
(add-hook 'window-setup-hook #'toggle-frame-maximized)

(add-to-list 'default-frame-alist '(undecorated . t))

;;; File Behavior and Backups
(setq make-backup-files nil
      create-lockfiles nil
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 1
      kept-old-versions 2
      version-control t
      backup-directory-alist '((".*" . "~/.emacs.d/saves")))


;; https://www.reddit.com/r/emacs/comments/1mrqi6p/emacs_toggle_transparency_with_interactive/
(defun horellana/toggle-frame-transparency ()
  "Toggle frame transparency with user-specified opacity value.
Prompts user whether to enable transparency. If yes, asks for opacity value (0-100).
If no, restores full opacity. Only affects the active frame."
  (set-frame-parameter nil 'alpha 80))

;;; Package System
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;;; Better Garbage Collection Management
(use-package gcmh
  :init (gcmh-mode 1))

;;; UI Enhancements
(use-package doom-themes :config (load-theme 'doom-snazzy t))
;; (use-package tao-theme :config (load-theme 'tao-yin t))
;; (use-package ef-themes :ensure t :config (load-theme  'ef-night t))
(use-package doom-modeline :defer t :init (doom-modeline-mode 1))
(use-package all-the-icons :after (doom-modeline) :if (display-graphic-p))
(use-package almost-mono-themes :defer t)

;;; Completion & Minibuffer
(use-package vertico :init (vertico-mode))
(use-package orderless
  :ensure
  :defer t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia :defer t :init (marginalia-mode))

(use-package consult
  :ensure
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
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
         ("M-r" . consult-history)))

(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init (setq prefix-help-command #'embark-prefix-help-command))
(use-package embark-consult)

(use-package corfu
  :ensure
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  :init

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  ;; Enable optional extension modes:
  (corfu-history-mode)
  (corfu-popupinfo-mode))

;;; Org Mode & Note-taking
(use-package org-roam
  :ensure t
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . consult-org-roam-file-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :custom (org-roam-directory (file-truename "~/Documents/org-roam"))
  :config (org-roam-db-autosync-enable))

(use-package consult-org-roam
  :ensure
  :after (org-roam)
  :config (consult-org-roam-mode 1))

(use-package flymake-flycheck
  :ensure t
  :hook ((flymake-mode . flymake-flycheck-auto)))

;;; Programming Experience
(use-package eglot
  :ensure t
  :defer t
  :hook ((python-ts-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)))

(use-package flymake :defer t)

(use-package eldoc-box
  :ensure
  :after eldoc
  :bind (("C-c K" . eldoc-box-help-at-point)))
(use-package smartparens-config
  :ensure smartparens
  :init (smartparens-global-mode))

(use-package yasnippet :defer t :config (yas-global-mode 1))

(use-package yasnippet-snippets :after (yasnippet))

(use-package which-key :defer t :init (which-key-mode))

;;; Evil & Modal Editing
(use-package evil :init (evil-mode 1))
(use-package evil-leader
  :after evil
  :config
  (evil-leader/set-key
    "k" 'kill-buffer
    "," 'evil-execute-in-emacs-state
    ";" 'comment-dwim
    "w" 'save-buffer)
  (global-evil-leader-mode))

(use-package evil-commentary
  :after evil
  :hook (prog-mode . evil-commentary-mode))

(use-package evil-org
  :ensure t
  :hook ((org-mode . evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;;; Project Tools
(use-package project :defer t)
(use-package magit
  :ensure
  :defer t
  :commands (magit-status))

(use-package plantuml-mode :ensure t)

(use-package jsdoc
  :ensure t
  :commands (jsdoc))

(eval-after-load "org"
  '(progn
     (setq org-log-done 'time
	   org-latex-compiler "xelatex")
     (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
     (setq org-plantuml-jar-path "~/Downloads/plantuml-1.2025.3.jar")))

(setq js-indent-level 2)

(eval-after-load "go-ts-mode"
  '(progn
     (defun eglot-gopls-before-save-hook ()
       (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
       (lambda ()
	 (call-interactively 'eglot-code-action-organize-imports)))

     (setq go-ts-mode-indent-offset 2)

     (add-hook 'before-save-hook #'eglot-format-buffer-on-save)))

;;; Performance Logging (optional)
(add-hook 'emacs-startup-hook
          (lambda ()
	    (add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-ts-mode))
	    (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
	    (add-to-list 'auto-mode-alist '("\\.sh\\'" . bash-ts-mode))
	    (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
	    (add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))
	    (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
	    (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
	    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))
	    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
	    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

	    (fset 'yes-or-no-p 'y-or-n-p)

	    (horellana/toggle-frame-transparency)))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-undo-system nil)
 '(package-selected-packages
   '(0blayout 0x0 all-the-icons almost-mono-themes cape cider
	      consult-org-roam consult-project-extra corfu csproj-mode
	      doom-modeline doom-themes ef-themes eldoc-box ellama
	      embark-consult evil-commentary evil-god-state
	      evil-leader evil-lisp-state evil-org flymake-eslint
	      flymake-flycheck gcmh go-mode haskell-mode hcl-mode
	      helpful jsdoc kaolin-themes kind-icon kotlin-mode magit
	      marginalia no-littering orderless ox-typst plantuml-mode
	      platformio-mode quelpa-use-package rust-mode sly
	      tao-theme treesit-auto vertico yaml-mode
	      yasnippet-snippets)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
