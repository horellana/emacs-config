(add-hook 'focus-out-hook 'garbage-collect)

(add-function :before after-focus-change-function
              (lambda ()
                (garbage-collect)))

(setq flymake-no-changes-timeout 1)

(setq eldoc-echo-area-prefer-doc-buffer 'maybe)
(setq eldoc-echo-area-use-multiline-p 1)
(setq-default indent-tabs-mode nil)

(progn
  (setq make-backup-files nil)
  (add-to-list 'global-mode-string '(" %i"))

  (setq package-native-compile t)
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))

  (blink-cursor-mode -1)
  (pixel-scroll-precision-mode)

  (when (not (eq system-type 'darwin))
    (menu-bar-mode -1))

  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (column-number-mode 1)
  ;; (global-hl-line-mode)
  (show-paren-mode 1)
  (eldoc-mode t)
  (setq tramp-default-method "ssh")
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
  (setf *no-smartparens-list* '(haskell-mode))
  (setf emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
  (setq create-lockfiles nil)
  (setf backup-by-copying t      ; don't clobber symlinks
        backup-directory-alist '(("~/.emacs.d/saves"))    ; don't litter my fs tree
        delete-old-versions t
        kept-new-versions 1
        kept-old-versions 2
        version-control t)
  (setq native-comp-async-jobs-number 1)

  (add-hook 'prog-mode-hook 'hs-minor-mode)
  (add-hook 'prog-mode-hook 'outline-minor-mode)

  (add-hook 'after-save-hook
            (lambda ()
              (let ((init-file (expand-file-name "~/.emacs.d/init.el")))
                (when (equal (buffer-file-name) init-file)
                  (byte-compile-file init-file)))))
  )

(set-face-attribute 'fringe nil
                    :foreground (face-foreground 'default)
                    :background (face-background 'default))

(fset 'yes-or-no-p 'y-or-n-p)

(use-package quelpa-use-package
  :ensure t)

(use-package no-littering
  :ensure t
  :defer t
  :config (progn
            (setq no-littering-etc-directory
                  (expand-file-name "config/" user-emacs-directory))

            (setq no-littering-var-directory
                  (expand-file-name "data/" user-emacs-directory))

            (setq auto-save-file-name-transforms
                  `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))))


(use-package smartparens
  :ensure t
  :config (progn
            (require 'smartparens-config)
            (add-hook 'prog-mode-hook #'smartparens-mode)))

(use-package which-key
  :ensure t
  :config (progn
            (which-key-mode)))


(progn
 (defun horellana/whitespace-cleanup-after-save ()
    (whitespace-cleanup))

 (add-hook 'after-save-hook 'horellana/whitespace-cleanup-after-save))
