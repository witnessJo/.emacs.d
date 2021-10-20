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
  (global-set-key (kbd "C-<tab>") 'company-complete)
  (add-hook 'after-init-hook 'global-company-mode)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  )

(use-package buffer-move
  :ensure t
  :config
  (setq buffer-move-behavior 'move)
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
  (smartparens-global-mode t)
  (sp-pair "(" ")" :wrap "C-(")
  (sp-pair "[" "]" :wrap "C-]")
  (sp-pair "\"" "\"" :wrap "C-\"")
  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)
  (setq sp-highlight-pair-overlay nil)
  )

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
			   (define-key yaml-mode-map "C-j" 'newline-and-indent))))

(use-package lsp-mode
  :hook
  (c-mode . lsp)
  (c++-mode-hook . lsp)
  (objc-mode-hook . lsp)
  (rust-mode-hook . lsp)
  (go-mode-hook . lsp)
  :config
  (setq lsp-eldoc-render-all nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-gopls-staticcheck t)
  (setq lsp-gopls-complete-unimported t)
  (setq lsp-modeline-diagnostics-scope :workspace)
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.500)

  
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-show-with-mouse t)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-imenu-enable t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-diagnostic-max-lines 8)
  (setq lsp-ui-peek-enable t)

  (setq lsp-ui-peek-find-workspace-symbol "pattern 0")
  )

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

(use-package treemacs
  :ensure t
  :config
  (progn
	(setq treemacs-position 'right
		  treemacs-width-is-initially-locked nil)
	)
  )


(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

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
  )

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
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
  (global-set-key (kbd "C-c g d") 'google-translate-at-point)
  :custom
  (google-translate-backend-method 'curl)
  )

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

(use-package auto-dim-other-buffers
  :ensure t
  :config
  (auto-dim-other-buffers-mode t)
  )

(use-package eshell-syntax-highlighting
  :ensure t
  :after esh-mode
  :demand t ;; Install if not already installed.
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode +1))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

(use-package prodigy
  :ensure t)

(use-package logview
  :ensure t)

(use-package sublimity
  :ensure t
  :config
  (setq sublimity-scroll-weight 5)
  (setq sublimity-scroll-drift-length 10)
  )

(use-package command-log-mode
  :ensure t)

(use-package undo-tree
  :ensure t
  :config
  (setq global-undo-tree-mode t))

(use-package bm
  :ensure t
  :demand t

  :init
  ;; restore on load (even before you require bm)
  (setq bm-restore-repository-on-load t)

  :config
  ;; Allow cross-buffer 'next'
  (setq bm-cycle-all-buffers t)

  ;; where to store persistant files
  (setq bm-repository-file "~/.emacs.d/bm-repository")

  ;; save bookmarks
  (setq-default bm-buffer-persistence t)

  ;; Loading the repository from file when on start up.
  (add-hook 'after-init-hook 'bm-repository-load)

  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)

  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))

  ;; The `after-save-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state.
  (add-hook 'after-save-hook #'bm-buffer-save)

  ;; Restoring bookmarks
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  ;; The `after-revert-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state. This hook might cause trouble when using packages
  ;; that automatically reverts the buffer (like vc after a check-in).
  ;; This can easily be avoided if the package provides a hook that is
  ;; called before the buffer is reverted (like `vc-before-checkin-hook').
  ;; Then new bookmarks can be saved before the buffer is reverted.
  ;; Make sure bookmarks is saved before check-in (and revert-buffer)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)


  :bind (("<f2>" . bm-next)
         ("S-<f2>" . bm-previous)
         ("C-<f2>" . bm-toggle))
  )

(use-package dockerfile-mode
  :ensure t)


(provide 'jong-packages)
