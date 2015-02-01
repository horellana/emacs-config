;;; The order in this matters
(ignore-errors 
 (load "/home/juiko/.emacs.d/config/cedet-config.el"))

(load "/home/juiko/.emacs.d/config/package-config.el")

(ignore-errors (package-initialize))

(require 'use-package)

(progn
  (add-to-list 'load-path "/home/juiko/.emacs.d/plugins/utilities")
  (cl-loop for d in (directory-files "~/.emacs.d/plugins")
	   do (add-to-list 'load-path (concat "/home/juiko/.emacs.d/plugins/" d)))
  (use-package utilities))

(require 'cl-lib)
(use-package f
  :ensure t)

(defun do-on-config (action &optional filter)
  (cl-loop for f in (cl-remove-if (lambda (f) (when filter (funcall filter f)))
				  (f-files "/home/juiko/.emacs.d/config/"))
	   do (funcall action f)))

(do-on-config (lambda (f) (load f)))

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

(run-with-idle-timer 10
                     t
                     (lambda () (ignore-errors (kill-buffer "*Buffer List*"))))

(use-package mpd)

(progn
  (setq-default indent-tabs-mode t)
  (setq-default tab-width 2)
  (setq-default standard-indent 2)
  (setq-default lisp-body-indent 2)

  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  
  (add-hook 'after-save-hook
            (lambda ()
              (let ((init-file (expand-file-name "~/.emacs")))
                (when (equal (buffer-file-name) init-file)
                  (byte-compile-file init-file)))))

  (setq inhibit-startup-message t)
  (global-set-key (kbd "C-<tab>") 'indent-whole-buffer)
  (global-set-key (kbd "M-_") 'hippie-expand)
  (global-set-key (kbd "C-.") 'clear-screen)
  (global-set-key (kbd "C-;") 'comment-dwim)

  (global-set-key (kbd "M-g M-g") 'goto-line-with-feedback)

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
 '(ac-auto-start 3)
 '(ac-candidate-limit 250)
 '(ac-delay 0.5)
 '(ac-dwim t)
 '(ac-use-fuzzy t)
 '(ansi-color-faces-vector
	 [default default default italic underline success warning error])
 '(ansi-color-names-vector
	 ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-safe-themes
	 (quote
		("b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" default)))
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(paradox-automatically-star t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
