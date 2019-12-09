(use-package js2-mode
  :ensure t)


(defun jong-nodejs-init-debug()
	(interactive)
	(require 'dap-node)
	(dap-node-setup))



(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . js2-mode))

(add-hook 'js-mode-hook 'js2-minor-mode)


(add-hook 'js2-mode-hook #'lsp)

(add-hook 'js-mode-hook #'lsp)
(add-hook 'typescript-mode-hook #'lsp) ;; for typescript support
(add-hook 'js3-mode-hook #'lsp) ;; for js3-mode support
(add-hook 'rjsx-mode #'lsp) ;; for rjsx-mode support


;; (setq lsp-clients-typescript-server "/usr/local/bin/typescript-language-server")
;; (setq lsp-clients-javascript-typescript-server "/usr/local/bin/typescript-language-server")

;; (use-package tern
;;   :ensure t)

;; (eval-after-load "tern"
;;   '(progn
;;      (define-key tern-mode-keymap (kbd "M-.") nil)
;;      (define-key tern-mode-keymap (kbd "M-,") nil)
;;      (define-key tern-mode-keymap (kbd "C-c C-b") nil)
;;      (define-key tern-mode-keymap (kbd "C-c C-r") nil)))

;; (use-package tide
;; :ensure t)

;; (defun setup-tide-mode ()
;; (interactive)
;; (tide-setup)
;; (flycheck-mode +1)
;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
;; (eldoc-mode +1)
;; (tide-hl-identifier-mode +1)
;; (company-mode +1))

;; (add-hook typescript-mode-hook 'lsp)






;; (defun jong-run-js ()
;; "jongyoungcha's run nodejs"
;; (interactive)
;; (pop-to-buffer (make-comint "Node Shell" "node" nil (buffer-file-name))))

;; (add-hook 'js2-mode-hook #'tern)
;; (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
;; (add-hook 'js2-mode-hook #'js2-refactor-mode)
;; (add-hook 'js2-mode-hook #'indium-interaction-mode)

;; (add-hook 'js2-mode-hook (lambda ()
;; (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; (add-hook 'js2-mode-hook (lambda ()
;; (tern-mode)
;; (company-mode)))

(add-hook 'js2-mode-hook (lambda () (interactive)
													(define-key js2-mode-map (kbd "C-c r s") 'tide-start-server)
													(define-key js2-mode-map (kbd "C-c r r") 'lsp-rename)
													(define-key js2-mode-map (kbd "C-c r .") 'lsp-find-definition)
													(define-key js2-mode-map (kbd "C-c r ,") 'lsp-find-references)
													(define-key js2-mode-map (kbd "C-c r i") 'lsp-find-implementation)
													(define-key js2-mode-map (kbd "C-c c c") 'jong-project-compile-project)
													(define-key js2-mode-map (kbd "C-c c r") 'jong-project-run-project)
													))

(provide 'jong-nodejs)
