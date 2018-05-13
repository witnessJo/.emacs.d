(use-package ensime
  :ensure t)


(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))


(add-hook 'sbt-mode-hook (lambda ()
                           (add-hook 'before-save-hook 'sbt-hydra:check-modified-buffers)))

(provide 'jong-scala)


