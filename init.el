;;; The order in this matters
(ignore-errors (package-initialize))

(require 'use-package)

(ignore-errors 
 (load "/home/juiko/.emacs.d/config/semantic-config.el")
 (load "/home/juiko/.emacs.d/config/package-config.el"))

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

(do-on-config 'load)

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
