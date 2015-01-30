;;; Code: 

(require 's)
(require 'dash)
(require 'projectile)

(defun perlp/lib-root (project-root)
	(format "%slib" project-root))

(defun perlp/lib-known? (project-lib-root)
	(-contains? (s-split ":" (getenv "PERL5LIB"))
							project-lib-root))

(defun perlp/update-perlib-lib! (project-root)
	(let ((lib-root (perlp/lib-root project-root)))
		(unless (perlp/lib-known? lib-root)
			(setenv "PERL5LIB"
							(format "%s:%s" 
											lib-root
											(getenv "PERL5LIB"))))))

(defun perlp/local-cpan? (project-root)
	nil)

(defun perlp/update-perlib-local-cpan! ()
	nil)

(defun perlp/setup-perl-project-lib ()
	(add-hook 'cperl-mode-hook
						(lambda ()
							(ignore-errors 
								(when (projectile-project-root)
									(perlp/update-perlib-lib! (projectile-project-root)))))))

(defun perlp/setup-perl-local-cpan ()
	(add-hook 'cperl-mode-hook
						(lambda ()
							(when (perlp/local-cpan? (projectile-project-root))
								(perlp/update-perlib-local-cpan!)))))

(provide 'perlp)


