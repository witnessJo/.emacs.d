;;; Code:

(require 'misc)
(require 'winner)
(winner-mode)

(message (regexp-opt-charset '(?a ?c ?e ? ?\t)))

(use-package auto-complete
  :ensure t)

(use-package company
  :ensure t
  :config
  (setq company-async-timeout 4)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0)
  ;; (setq company-minimum-prefix-length 5)
  (global-set-key (kbd "C-<tab>") 'company-complete)
  (add-hook 'after-init-hook 'global-company-mode)
  ;; (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  )

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode)
  (setq company-quickhelp-delay nil)
  (define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin)
  )

(use-package magit-delta
  :ensure t)

(use-package magit
  :ensure t
  :config
  (setq magit-delta-mode t)
  (setq git-commit-summary-max-length 1000)
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
  )

(use-package projectile
  :ensure t
  :init
  :config
  (projectile-mode 1)
  (setq projectile-globally-ignored-directories (append '(".git") projectile-globally-ignored-directories))
  (setq projectile-globally-ignored-directories (append '(".svn") projectile-globally-ignored-directories))
  (setq projectile-enable-caching t)
  (setq projectile-git-submodule-command nil))

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
  (setq lsp-eldoc-render-all nil)
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
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-diagnostic-max-lines 8)
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-peek-find-workspace-symbol "pattern 0")
  )

(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :init)

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

(use-package avy
  :ensure t
  :config
  :bind
  ("C-'" . avy-goto-word-0)
  ("C-;" . avy-goto-line))


(use-package counsel
  :ensure t
  :after ivy
  :config
  (counsel-mode)
  (setq counsel-find-file-at-point t))

(use-package ivy
  :ensure t
  :defer 0.1
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  (setq ivy-initial-inputs-alist nil)
  (add-to-list 'ivy-re-builders-alist
			   '(counsel-M-x . ivy--regex-ignore-order)))

(use-package ag
  :ensure t)

(use-package counsel-projectile
  :ensure t)

(use-package ivy-posframe
  :ensure t
  :demand t
  :after ivy
  :custom
  (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  :config
  (setq ivy-posframe-parameters
		'((left-fringe . 12)
		  (right-fringe . 12)))
  (ivy-posframe-mode))

(use-package google-translate
  :ensure t
  :config
  (setq google-translate-default-source-language "en")
  (setq google-translate-default-target-language "ko")
  (setq google-translate-show-phonetic 1)
  (global-set-key (kbd "C-c g d") 'google-translate-at-point))

(use-package org
  :ensure t)

(use-package markdown-mode
  :ensure t  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
		 ("\\.md\\'" . markdown-mode)
		 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package evil
  :ensure t)

(use-package async
  :ensure t
  :config
  (async-bytecomp-package-mode 1))

(use-package hungry-delete
  :ensure t)

(use-package syntax-subword
  :ensure t
  :config)

;; When the loading time, the packages will be updated.
; (use-package auto-package-update
; :ensure t
; :config
; (auto-package-update-now))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package prodigy
  :ensure t)


(provide 'jong-packages)
