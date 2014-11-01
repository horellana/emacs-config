(use-package sh-script
             :commands (sh-mode)
             :config
             (progn
               (use-package flycheck
                            :config (progn
                                      (add-hook 'sh-mode-hook 'flycheck-mode)))
               (use-package ac-sh
                            :config (progn
                                      (add-hook 'sh-mode-hook
                                                (lambda ()
                                                  (make-local-variable 'ac-sources)
                                                  (setq-local ac-sources
                                                              '(ac-source-words-in-buffer
                                                                ac-source-words-in-same-mode-buffers
                                                                ac-source-sh-completion
                                                                ac-source-sh-completion-shell-keywords
                                                                ac-source-sh-completion-shell-variables))))

                                      (add-hook 'sh-mode-hook
                                                (lambda ()  (auto-complete-mode)))))))

(use-package shell
             :commands (shell-mode eshell shell)
             :config (progn
                       (use-package ac-sh
                                    :config (progn
                                              (add-hook 'shell-mode-hook
                                                        (lambda ()
                                                          (make-local-variable 'ac-sources)
                                                          (setq-local ac-sources
                                                                      '(ac-source-words-in-buffer
                                                                        ac-source-words-in-same-mode-buffers
                                                                        ac-source-sh-completion
                                                                        ac-source-sh-completion-shell-keywords
                                                                        ac-source-sh-completion-shell-variables))))))

                       (add-hook 'shell-mode-hook
                                 (lambda ()  (auto-complete-mode)))))



(provide 'shell-config)
