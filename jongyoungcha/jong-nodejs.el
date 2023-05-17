(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  )

(use-package js2-mode
  :ensure t)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook
  (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 4)
  (require 'typescript-mode))

(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)

(defun my-setup-dap-node ()
  "Require dap-node feature and run dap-node-setup if VSCode module isn't already installed"
  (require 'dap-node)
  (unless (file-exists-p dap-node-debug-path) (dap-node-setup)))

(add-hook 'typescript-mode-hook 'my-setup-dap-node)
(add-hook 'javascript-mode-hook 'my-setup-dap-node)

(setq lsp-eslint-server-command 
      '("node"
        "/home/USER/.vscode/extensions/dbaeumer.vscode-eslint-2.0.11/server/out/eslintServer.js" 
        "--stdio"))

(setq lsp-disabled-clients '(jsts-ls eslint))

(add-hook 'typescript-mode-hook 'lsp-deferred)
(add-hook 'javascript-mode-hook 'lsp-deferred)

(add-hook 'web-mode-hook #'lsp)
(add-hook 'js-mode-hook #'lsp)
(add-hook 'js2-mode-hook #'lsp)
(add-hook 'js3-mode-hook #'lsp) ;; for js3-mode support
(add-hook 'typescript-mode-hook #'lsp) ;; for typescript support
(add-hook 'rjsx-mode #'lsp) ;; for rjsx-mode support

(add-hook 'web-mode-hook 'jo/lsp-key-bindings)
(add-hook 'js-mode-hook 'jo/lsp-key-bindings)
(add-hook 'js2-mode-hook 'jo/lsp-key-bindings)
(add-hook 'js3-mode-hook 'jo/lsp-key-bindings)
(add-hook 'typescript-mode-hook 'jo/lsp-key-bindings)
(add-hook 'rjsx-mode 'jo/lsp-key-bindings) ;; for rjsx-mode support


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
