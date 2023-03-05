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
  (setq company-idle-delay 0.0))

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

(use-package hydra
  :ensure t)

(defhydra Navigation (global-map "<f>")
  "testnn"
  :Line
  ("n" next-line)
  ("p" previous-line)
  ("a" beginning-of-line)
  ("e" move-end-of-line)
  ("g" goto-line)
  :Word
  ("f" forward-word "Next")
  ("b" backward-word "Previous")
  ("{" org-backward-element "Next Element")
  ("}" org-forward-element "Previous Element")
  :Screen
  ("v" scroll-up-command "Scroll Down")
  ("V" scroll-down-command "Scroll Up")
  ("l" recenter-top-bottom "Center Page")
  ("r" move-to-window-line-top-bottom "Relocate Point")
  ("m" helm-imenu "Textual Menu"))

(defhydra hydra-zoom (global-map "C-=")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))


;; (use-package hydra-goto
;;   :doc "Search and move cursor"
;;   :bind ("M-j" . *hydra-goto/body)
;;   :pretty-hydra
;;   ((:title " Goto" :color blue :quit-key "q" :foreign-keys warn :separator "-")
;;    ("Got"
;;     (("i" avy-goto-char       "char")
;;      ("t" avy-goto-char-timer "timer")
;;      ("w" avy-goto-word-2     "word")
;;      ("j" avy-resume "resume"))
;;     "Line"
;;     (("h" avy-goto-line        "head")
;;      ("e" avy-goto-end-of-line "end")
;;      ("n" consult-goto-line    "number"))
;;     "Topic"
;;     (("o"  consult-outline      "outline")
;;      ("m"  consult-imenu        "imenu")
;;      ("gm" consult-global-imenu "global imenu"))
;;     "Error"
;;     ((","  flycheck-previous-error "previous" :exit nil)
;;      ("."  flycheck-next-error "next" :exit nil)
;;      ("l" consult-flycheck "list"))
;;     "Spell"
;;     ((">"  flyspell-goto-next-error "next" :exit nil)
;;      ("cc" flyspell-correct-at-point "correct" :exit nil)))))


(use-package quickrun
  :ensure t)


(provide 'jong-packages)
