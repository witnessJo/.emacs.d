
(use-package eglot
  :ensure t
  :config
  (defun jo/eglot-key-bindings()
    (setq indent-tabs-mode t)
    ;; setting company-go mode...
    ;; (setq company-tooltip-limit 20)
    (setq company-echo-delay 0)
    (setq company-begin-commands '(self-insert-command))
    (local-set-key (kbd "A-<tab>") 'copilot-complete)
    (local-set-key (kbd "C-c h") 'xre)
    (local-set-key (kbd "C-c r w") 'eglot-reconnect)
    (local-set-key (kbd "C-c C-c") 'helm-lsp-code-actions)
    (local-set-key (kbd "M-.") 'xref-find-definitions)
    (local-set-key (kbd "M-,") 'xref-pop-marker-stack)
    (local-set-key (kbd "C-,") 'xref-go-back)
    (local-set-key (kbd "C-.") 'xref-go-forward)
    (local-set-key (kbd "C-c r ,") 'xref-find-references)
    (local-set-key (kbd "C-c r i") 'eglot-find-implementation)
    (local-set-key (kbd "C-c o i") 'eglot-code-action-organize-imports)
    (local-set-key (kbd "C-c r l") 'counsel-imenu)
    (local-set-key (kbd "C-c c c") 'jong-project-compile-project)
    (local-set-key (kbd "C-c r r") 'eglot-rename)
    (eglot-ensure)
    )
  )


(provide 'jong-eglot)
