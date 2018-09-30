(use-package js2-mode
  :ensure t)

(use-package js2-refactor
  :ensure t)

(use-package xref-js2
  :ensure t)

(use-package nodejs-repl
  :ensure t)

(use-package js-comint
  :ensure t)

(use-package indium
  :ensure t)

;; (use-package tern
;;   :ensure t)

;; (eval-after-load "tern"
;;   '(progn
;;      (define-key tern-mode-keymap (kbd "M-.") nil)
;;      (define-key tern-mode-keymap (kbd "M-,") nil)
;;      (define-key tern-mode-keymap (kbd "C-c C-b") nil)
;;      (define-key tern-mode-keymap (kbd "C-c C-r") nil)))

(use-package tide
  :ensure t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(setq company-tooltip-align-annotations t)
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'js2-mode-hook #'setup-tide-mode)
(flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; (use-package company-tern
;;   :ensure t)
;; (add-to-list 'company-backends 'company-tern)

(defun jong-run-js ()
  "jongyoungcha's run nodejs"
  (interactive)
  (pop-to-buffer (make-comint "Node Shell" "node" nil (buffer-file-name))))

;; (add-hook 'js2-mode-hook #'tern)
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(add-hook 'js2-mode-hook #'indium-interaction-mode)

(add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(add-hook 'js2-mode-hook (lambda ()
                           ;; (tern-mode)
                           (company-mode)))

(add-hook 'js2-mode-hook
          (lambda ()
            (define-key js2-mode-map (kbd "C-c r s") 'tide-start-server)
            (define-key js2-mode-map (kbd "C-c r .")
	      (lambda () (interactive)
		(call-interactively 'set-mark-command)
		(call-interactively 'set-mark-command)
		(call-interactively 'tide-jump-to-definition)))
            (define-key js2-mode-map (kbd "C-c r ,")
	      (lambda () (interactive)
		(call-interactively 'set-mark-command)
		(call-interactively 'set-mark-command)
		(call-interactively 'tide-references)))
            (define-key js2-mode-map (kbd "C-c r r") 'tide-rename-symbol)
            (define-key js2-mode-map (kbd "C-c r d") 'tide-documentation-at-point)
            (define-key js2-mode-map (kbd "C-c d l") 'indium-eval-last-node)
            (define-key js2-mode-map (kbd "C-c d r") 'indium-eval-region)
            (define-key js2-mode-map (kbd "C-c d b") 'indium-eval-buffer)
            (define-key js2-mode-map (kbd "C-c d :") 'indium-inspect-expression)
            (define-key js2-mode-map (kbd "C-c c c") 'jong-run-js)
            (define-key js2-mode-map (kbd "C-c r r")
              (lambda () (interactive)
                (call-interactively 'nodejs-repl-send-last-expression)
                (nodejs-repl-switch-to-repl)
                (other-window -1)))
            (define-key js2-mode-map (kbd "C-c r g")
              (lambda () (interactive)
                (call-interactively 'nodejs-repl-send-region)
                (nodejs-repl-switch-to-repl)
                (other-window -1)))
            (define-key js2-mode-map (kbd "C-c r b")
              (lambda () (interactive)
                (call-interactively 'nodejs-repl-send-buffer)
                (nodejs-repl-switch-to-repl)
                (other-window -1)))
            (define-key js2-mode-map (kbd "C-c r l")
              (lambda () (interactive)
                (call-interactively 'nodejs-repl-send-line)
                (nodejs-repl-switch-to-repl)
                (other-window -1))))
          )

(provide 'jong-nodejs)
