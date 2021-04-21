(req-package magit
  :defer t
  :ensure t)

(eval-after-load "evil-leader"
  '(progn
     (evil-leader/set-key
       "mgb" 'magit-branch
       "mgc" 'magit-checkout
       "mgc" 'magit-checkout
       "mgl" 'magit-log
       "mgs" 'magit-status
       "mgpl" 'magit-pull
       "mgps" 'magit-push)))
