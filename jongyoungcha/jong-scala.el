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


(provide 'jong-scala)
