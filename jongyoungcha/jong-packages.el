;;; Code:

(require 'misc)

(message (regexp-opt-charset '(?a ?c ?e ? ?\t)))

;; add themes
(use-package color-theme-sanityinc-tomorrow :ensure t)
(use-package solarized-theme :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-popup-type 'minibuffer))


(use-package clang-format
	:ensure t
	:config )


(use-package yaml-mode
	:ensure t
	:config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
	(add-hook 'yaml-mode-hook
			      '(lambda ()
							 (define-key yaml-mode-map "\C-m" 'newline-and-indent))))


(provide 'jong-packages)
