
(use-package lsp-mode
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
  :commands lsp)

(use-package lsp-ui
  :ensure t  :config
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

