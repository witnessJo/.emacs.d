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
  :after (lsp-mode)
  :bind
  (:map company-active-map
		("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :config
  (setq company-async-timeout 4)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0)
  (setq company-show-numbers t)
  )

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))


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
  (setq projectile-git-submodule-command nil)
  (setq projectile-track-known-projects-automatically nil))

;; add themes
(use-package color-theme-sanityinc-tomorrow :ensure t)
(use-package solarized-theme :ensure t)

(use-package yasnippet
  :ensure t
  :config
  ;; (setq yas-global-mode 1)
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :ensure t)

(use-package helm-c-yasnippet
  :ensure t
  :config
  (setq helm-yas-space-match-any-greedy t)
  )

(use-package smartparens
  :ensure t
  :bind
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (progn (show-smartparens-global-mode t))
  (setq sp-navigate-close-if-unbalanced t)
  (sp-pair "(" ")" :wrap "C-(")
  (sp-pair "(" ")" :wrap "C-(")
  (sp-pair "[" "]" :wrap "C-]")
  (sp-pair "{" "}" :wrap "C-{")
  (sp-pair "\"" "\"" :wrap "C-\"")
  (sp-pair "'" "'" :wrap "C-'")
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
  )

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

(use-package load-env-vars
  :ensure t)

(use-package dotenv-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode)))

(use-package json-mode
  :ensure t)

(use-package flyspell
  :ensure t)

(defun flyspell-english ()
  (interactive)
  (ispell-change-dictionary "default")
  (flyspell-buffer))

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
  (setq avy-all-windows nil)
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

(use-package wgrep
  :ensure t)

(use-package ag
  :ensure t)

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
  ;; (ivy-posframe-mode)
  )

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
  ;; (auto-dim-other-buffers-mode t)
  )

(use-package eshell-syntax-highlighting
  :ensure t
  :after esh-mode
  :demand t ;; Install if not already installed.
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode +1))

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

(use-package dockerfile-mode
  :ensure t)


(use-package groovy-mode
  :ensure t)

(use-package vdiff
  :ensure t)

(use-package vdiff-magit
  :ensure t
  :config
  (require 'vdiff-magit)
  (define-key magit-mode-map "e" 'vdiff-magit-dwim)
  (define-key magit-mode-map "E" 'vdiff-magit)
  (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
  (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
  (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
  (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit))

(use-package ssh-config-mode
  :ensure t)

(use-package gpt
  :ensure t
  :config
  (setq gpt-openai-key (getenv "GPT_KEY")))

(use-package gptel
  :ensure t
  :config
  (setq gptel-api-key (getenv "GPT_KEY"))
  (defun jo/gptel-toggle-gptel()
    (interactive)
    (let ((gptel-buffer-name "*ChatGPT*")
          (gptel-window))
      (if (setq gptel-window (get-buffer-window gptel-buffer-name))
          (delete-window gptel-window)
        (call-interactively 'gptel)
        )
      ))
  )

(use-package hydra
  :ensure t)

(use-package quickrun
  :ensure t)

(use-package smex
  :ensure t
  :config
  (smex-initialize))

(use-package helm-smex
  :ensure t)

(use-package string-inflection
  :ensure t
  :config
  (defun jong-package-string-inflection-all-cycle ()
    (interactive)
    (cond
     ;; for emacs-lisp-mode
     ((eq major-mode 'emacs-lisp-mode)
      (string-inflection-all-cycle))
     ;; for python
     ((eq major-mode 'python-mode)
      (string-inflection-python-style-cycle))
     ;; for java
     ((eq major-mode 'java-mode)
      (string-inflection-java-style-cycle))
     ;; for golang
     ((eq major-mode 'go-mode)
      (string-inflection-python-style-cycle))
     ;; for elixir
     ((eq major-mode 'elixir-mode)
      (string-inflection-elixir-style-cycle))
     (t
      ;; default
      (string-inflection-ruby-style-cycle)))
    )
  )

(use-package bufler
  :ensure t
  :init
  :config
  (bufler-mode 1)
  (bufler-tabs-mode 1)
  (tab-bar-mode 1)
  (tab-line-mode 1)
  (add-to-list 'window-state-change-functions
               (lambda() (call-interactively 'bufler-workspace-focus-buffer)))
  (add-hook 'window-configuration-change-hook
            (lambda() (call-interactively 'bufler-workspace-focus-buffer)))
  (setq bufler-workspace-set-hook nil)
  (bufler-defgroups
    (group
     ;; Subgroup collecting all named workspaces.
     (auto-workspace))
    (group
     ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
     (group-or "*Help/Info*"
               (mode-match "*Help*" (rx bos "help-"))
               (mode-match "*Info*" (rx bos "info-"))))
    (group
     ;; Subgroup collecting all special buffers (i.e. ones that are not
     ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
     ;; through to other groups, so they end up grouped with their project buffers).
     (group-and "*Special*"
                (lambda (buffer)
                  (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
                                       buffer)
                              (funcall (mode-match "Dired" (rx bos "dired"))
                                       buffer)
                              (funcall (auto-file) buffer))
                    "*Special*")))
     (group
      ;; Subgroup collecting these "special special" buffers
      ;; separately for convenience.
      (name-match "**Special**"
                  (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
     (group
      ;; Subgroup collecting all other Magit buffers, grouped by directory.
      (mode-match "*Magit* (non-status)" (rx bos (or "magit" "forge") "-"))
      (auto-directory))
     ;; Subgroup for Helm buffers.
     (mode-match "*Helm*" (rx bos "helm-"))
     ;; Remaining special buffers are grouped automatically by mode.
     (auto-mode))
    ;; All buffers under "~/.emacs.d" (or wherever it is).
    (dir user-emacs-directory)
    (group
     ;; Subgroup collecting buffers in `org-directory' (or "~/org" if
     ;; `org-directory' is not yet defined).
     (dir (if (bound-and-true-p org-directory)
              org-directory
            "~/org"))
     (group
      ;; Subgroup collecting indirect Org buffers, grouping them by file.
      ;; This is very useful when used with `org-tree-to-indirect-buffer'.
      (auto-indirect)
      (auto-file))
     ;; Group remaining buffers by whether they're file backed, then by mode.
     (group-not "*special*" (auto-file))
     (auto-mode))
    (group
     ;; Subgroup collecting buffers in a projectile project.
     (auto-projectile))
    (group
     ;; Subgroup collecting buffers in a version-control project,
     ;; grouping them by directory.
     (auto-project))
    ;; Group remaining buffers by directory, then major mode.
    (auto-directory)
    (auto-mode))
  )

(use-package helm-bufler
  :ensure t)


(provide 'jong-packages)
