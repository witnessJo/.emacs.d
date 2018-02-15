(gnutls-available-p)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
			 '("marmalade" . "https://marmalade-repo.org/packages/"))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)
	
(use-package xscheme :ensure t)

(message-box "test message")

(add-hook 'scheme-mode-hook
		  (lambda()
			(local-set-key (kbd "C-c c c") 'run-scheme))
		  )

(provide 'jong-scheme);;
