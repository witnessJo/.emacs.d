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


(use-package lsp-mode
  :hook
  (c-mode . lsp)
  (c++-mode-hook . lsp)
  (objc-mode-hook . lsp)
	(rust-mode-hook . lsp)
	(go-mode-hook . lsp)
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally
;; (use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp
  :ensure t
  :commands company-lsp
  :config
  (eval-after-load 'company
	'(add-to-list
	  'company-backends 'company-lsp)))
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)


(provide 'jong-packages)
