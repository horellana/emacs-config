(use-package bind-key
  :ensure t)

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
				      org-mode
				      magit-mode)
			do (evil-set-initial-state mode 'emacs))

	       (evil-mode))))

(use-package evil-smartparens
  :ensure t
  :requires (evil smartparens)
  :hook (smartparens-mode . evil-smartparens-mode))

(use-package evil-commentary
  :ensure t
  :requires (evil)
  :config (progn
	    (evil-commentary-mode)))

(use-package evil-god-state
  :ensure t
  :commands (evil-execute-in-emacs-state))

(use-package evil-leader
  :ensure t
  :requires evil
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
  :init (progn
	  (setf evil-lisp-state-global t)
	  (setf evil-lisp-state-enter-lisp-state-on-command nil))

  :config (eval-after-load "evil-lisp-state"
	    '(progn
	       (bind-key "L" 'evil-lisp-state evil-normal-state-map))))
