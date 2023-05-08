
(use-package lsp-mode
  :ensure t
  :hook
  (c-mode . lsp)
  (c++-mode-hook . lsp)
  (objc-mode-hook . lsp)
  (rust-mode-hook . lsp)
  (go-mode-hook . lsp)
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-eldoc-render-all nil)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-go-library-directories-include-go-modules t)
  (setq lsp-gopls-staticcheck t)
  (setq lsp-gopls-complete-unimported t)
  (setq lsp-modeline-diagnostics-scope :workspace)
  (setq gc-cons-threshold 100000000)
  (setq lsp-response-timeout 100)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-file-watch-threshold 10000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-idle-delay 0.500)

  (defun jo/lsp-key-bindings()
    (setq indent-tabs-mode t)
    ;; setting company-go mode...
    ;; (setq company-tooltip-limit 20)
    (setq company-echo-delay 0)
    (setq company-begin-commands '(self-insert-command))
    (local-set-key (kbd "A-<tab>") 'copilot-complete)
    (local-set-key (kbd "C-c h") 'lsp-ui-doc-show)
    (local-set-key (kbd "C-c r w") 'lsp-workspace-restart)
    (local-set-key (kbd "C-c C-c") 'helm-lsp-code-actions)
    (local-set-key (kbd "M-.") 'lsp-ui-peek-find-definitions)
    (local-set-key (kbd "M-,") 'xref-pop-marker-stack)
    (local-set-key (kbd "C-,") 'xref-go-back)
    (local-set-key (kbd "C-.") 'xref-go-forward)
    (local-set-key (kbd "C-c r ,") 'lsp-ui-peek-find-references)
    (local-set-key (kbd "C-c r i") 'lsp-ui-peek-find-implementation)
    (local-set-key (kbd "C-c o i") 'lsp-ui-organize-imports)
    (local-set-key (kbd "C-c r l") 'counsel-imenu)
    (local-set-key (kbd "C-c c c") 'jong-project-compile-project)
    (local-set-key (kbd "C-c r r") 'lsp-rename)
    (lsp)
    )
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :config
  ;; (setq lsp-ui-doc-enable nil)
  ;; (setq lsp-ui-doc-position 'at-point)
  ;; (setq lsp-ui-doc-show-with-cursor nil)
  ;; (setq lsp-ui-doc-delay 2)
  
  (lsp-ui-mode t)
  ;; (setq lsp-ui-imenu-enable t)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-diagnostic-max-lines 8)
  (setq lsp-ui-peek-enable t)
  
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-peek-find-workspace-symbol "pattern 0")
  )

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)


(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)


(provide 'jong-lsp)

