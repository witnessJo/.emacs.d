(gnutls-available-p)

(require 'package)

(use-package xscheme
  :ensure t)

(use-package geiser
  :ensure t)

(use-package geiser-mit
  :ensure t)

(setq geiser-active-implementations '(guile))
(setq geiser-guile-binary (or (executable-find "guile")
                              (executable-find "/usr/bin/guile")
                              (executable-find "/usr/local/bin/guile")
                              (executable-find "/opt/homebrew/bin/guile")
                              "guile"))

(add-hook 'scheme-mode-hook
		  (lambda()
			(local-set-key (kbd "C-c c c") 'xscheme-send-buffer))
		  )

(provide 'jong-scheme)
