(gnutls-available-p)

(require 'package)

(use-package xscheme :ensure t)

(add-hook 'scheme-mode-hook
		  (lambda()
			(local-set-key (kbd "C-c c c") 'xscheme-send-buffer))
		  )

(provide 'jong-scheme)
