(require 'tramp)
(use-package tramp-term
  :ensure t)
(use-package counsel-tramp
  :ensure t)

(setq tramp-default-method "ssh")
(add-to-list 'tramp-remote-path '~/goworks/bin/)

(add-hook 'dired-mode-hook (lambda()
                            (local-set-key "C-c t t" 'tramp-term)
                            ))

(provide 'jong-tramp)
