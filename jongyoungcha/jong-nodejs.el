(use-package js2-mode :ensure t)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . js2-mode))

(add-hook 'js-mode-hook 'js2-minor-mode)

(setq lsp-eslint-server-command 
   '("node" 
     "/home/USER/.vscode/extensions/dbaeumer.vscode-eslint-2.0.11/server/out/eslintServer.js" 
     "--stdio"))


(add-hook 'js2-mode-hook #'lsp)

(add-hook 'js-mode-hook #'lsp)
(add-hook 'typescript-mode-hook #'lsp) ;; for typescript support
(add-hook 'js3-mode-hook #'lsp) ;; for js3-mode support
(add-hook 'rjsx-mode #'lsp) ;; for rjsx-mode support

(add-hook 'js2-mode-hook (lambda () (interactive)
						   (setq indent-tabs-mode t)
						   (setq tab-width 4)
						   (define-key js2-mode-map (kbd "C-c r s") 'tide-start-server)
						   (define-key js2-mode-map (kbd "C-c r r") 'lsp-rename)
						   (define-key js2-mode-map (kbd "C-c r .") 'lsp-find-definition)
						   (define-key js2-mode-map (kbd "C-c r ,") 'lsp-find-references)
						   (define-key js2-mode-map (kbd "C-c r i") 'lsp-find-implementation)
						   (define-key js2-mode-map (kbd "C-c c c") 'jong-project-compile-project)
						   (define-key js2-mode-map (kbd "C-c c r") 'jong-project-run-project)
						   ))

(provide 'jong-nodejs)
