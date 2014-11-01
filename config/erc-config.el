(use-package erc
             :config (progn
                       (erc-autojoin-mode 1)
											 
                       (setq erc-autojoin-channels-alist
                             '(("freenode.net"
                                "#emacs"
                                "#gentoo"
                                "#stumpwm"
                                "#haskell"
                                "#concatenative"
                                "#perl"
                                "#lisp"
																"#archlinux")
															 ("torncity.com"
																"#lobby"
																"#help")
															 ("perl.org"
																"#p5p")))))

(provide 'erc-config)
