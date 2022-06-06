;;; Code:

(require 'misc)
(require 'winner)
(winner-mode)

(message (regexp-opt-charset '(?a ?c ?e ? ?\t)))


(use-package so-long
  :ensure t
  :config
  (global-so-long-mode t))

(use-package auto-complete
  :ensure t
  )

(use-package company
  :ensure t
  :bind (:map company-active-map
			  ("<tab>" . company-complete-selection))
  :config
  (setq company-async-timeout 4)
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.02))

(add-hook 'after-init-hook 'global-company-mode)

(use-package buffer-move
  :ensure t
  :config
  (setq buffer-move-behavior 'swap)
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

(use-package treemacs
  :ensure t
  :commands (treemacs
             treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-fringe-indicator-mode)
  :init
  (progn
	(setq treemacs-position 'right
		  treemacs-width-is-initially-locked nil)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t))
  )


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
  
  (defun jong-avy-goto-line ()
    (interactive)
    (progn
      (call-interactively 'avy-goto-line)
      (call-interactively 'recenter)
      )
    )
  
  (defun jong-avy-goto-word-1 ()
    (interactive)
    (progn
      (call-interactively 'avy-goto-word-1)
      (call-interactively 'recenter)
      )
    )
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
  :ensure t
  :config
  ;; (global-command-log-mode t)
  )

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (defun jong-undo-tree-clear ()
	(interactive)
	(setq buffer-undo-tree nil)
	)
  (setq undo-tree-auto-save-history nil)
  )


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


(use-package groovy-mode
  :ensure t)


(provide 'jong-packages)
