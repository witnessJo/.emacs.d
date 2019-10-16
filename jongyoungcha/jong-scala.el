(use-package scala-mode
	:ensure t
	:mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
	:ensure t
	:commands sbt-start sbt-command
	:config
	;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
	;; allows using SPACE when in the minibuffer
	(substitute-key-definition
	 'minibuffer-complete-word
	 'self-insert-command
	 minibuffer-local-completion-map))

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
	:ensure t
	:init (global-flycheck-mode))

(use-package lsp-mode
	:ensure t
	;; Optional - enable lsp-mode automatically in scala files
	:hook (scala-mode . lsp)
	:config (setq lsp-prefer-flymake nil))


;; Add company-lsp backend for metals
;; (use-package company-lsp
;; :ensure t)

(provide 'jong-scala)
