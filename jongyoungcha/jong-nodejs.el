(use-package js2-mode
  :ensure t)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook
  (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)
  (require 'typescript-mode))

(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)

(setq lsp-eslint-server-command 
      '("node"
        "/home/USER/.vscode/extensions/dbaeumer.vscode-eslint-2.0.11/server/out/eslintServer.js" 
        "--stdio"))

(setq lsp-disabled-clients '(jsts-ls eslint))

(add-hook 'web-mode-hook #'lsp)
(add-hook 'js-mode-hook #'lsp)
(add-hook 'js2-mode-hook #'lsp)
(add-hook 'js3-mode-hook #'lsp) ;; for js3-mode support
(add-hook 'typescript-mode-hook #'lsp) ;; for typescript support
(add-hook 'rjsx-mode #'lsp) ;; for rjsx-mode support

(add-hook 'web-mode-hook #'jong-js-key-set)
(add-hook 'js-mode-hook #'jong-js-key-set)
(add-hook 'js2-mode-hook #'jong-js-key-set)
(add-hook 'js3-mode-hook #'jong-js-key-set)
(add-hook 'typescript-mode-hook #'jong-js-key-set)
(add-hook 'rjsx-mode #'jong-js-key-set) ;; for rjsx-mode support

(defun jong-js-key-set()
  "Key set for javacript."
  (setq indent-tabs-mode t)
  (setq tab-width 2)
  (local-set-key (kbd "C-c r w") 'lsp-restart-workspace)
  (local-set-key (kbd "C-c r r") 'lsp-rename)
  (local-set-key (kbd "C-c r l") 'counsel-imenu)
  (local-set-key (kbd "C-c o i") 'lsp-ui-organize-imports)
  (local-set-key (kbd "C-c r .") 'lsp-ui-peek-find-definitions)
  (local-set-key (kbd "C-c r ,") 'lsp-ui-peek-find-references)
  (local-set-key (kbd "C-c r i") 'lsp-ui-peek-find-implementation)
  (local-set-key (kbd "C-c C-c") 'helm-lsp-code-actions)
  (local-set-key (kbd "C-c c c") 'jong-project-compile-project)
  (local-set-key (kbd "C-c c r") 'jong-project-run-project)
  )

(provide 'jong-nodejs)
