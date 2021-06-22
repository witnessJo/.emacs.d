;;; Code:

(require 'misc)
(require 'winner)
(winner-mode)


(message (regexp-opt-charset '(?a ?c ?e ? ?\t)))

;; add themes
(use-package color-theme-sanityinc-tomorrow :ensure t)
(use-package solarized-theme :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (setq yas-global-mode 1))

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (setq which-key-popup-type 'minibuffer))

(use-package clang-format
  :ensure t)

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
  :config
  ;; (setq lsp-prefer-flymake nil)
  (setq lsp-eldoc-render-all nil)
  ;; (setq lsp-signature-render-all nil)
  ;; (setq lsp-modeline-diagnostics-scope :workspace)
  (setq lsp-modeline-diagnostics-scope :workspace)
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.500)
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-imenu-enable t)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-diagnostic-max-lines 8)
  )

(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :init)
;; (global-whitespace-cleanup-mode)
;; (add-hook 'after-save-hook 'whitespace-cleanup))

(use-package dotenv-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode)))

(use-package json-mode
  :ensure t)

(use-package flyspell
  :ensure t)

(use-package multiple-cursors
  :ensure t)

(use-package org
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.org$" . org-mode)))

(use-package auto-highlight-symbol
  :ensure t
  :config
  (global-auto-highlight-symbol-mode t))

(use-package helm-ag
  :ensure t)
  

(provide 'jong-packages)
