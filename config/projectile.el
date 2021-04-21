(req-package projectile
  :ensure t)

(eval-after-load "evil-leader"
  '(progn
     (evil-leader/set-key
       "ptp" 'projectile-test-project)))
