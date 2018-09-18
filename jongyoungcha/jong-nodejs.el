
(use-package js2-mode
  :ensure t)
(use-package js2-refactor
  :ensure t)
(use-package xref-js2
  :ensure t)
;; (use-package indium
;;   :ensure t)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(use-package tern
  :ensure t)
(eval-after-load "tern"
  '(progn
     (define-key tern-mode-keymap (kbd "M-.") nil)
     (define-key tern-mode-keymap (kbd "M-,") nil)
     (define-key tern-mode-keymap (kbd "C-c C-b") nil)
     (define-key tern-mode-keymap (kbd "C-c C-r") nil)))


(use-package nodejs-repl
  :ensure t
  :bind())

(use-package company-tern
  :ensure t)
(add-to-list 'company-backend 'company-tern)

(add-hook 'js2-mode-hook #'tern-mode)
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(add-hook 'js2-mode-hook #'js2-refactor-mode)

(add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))
(add-hook 'js2-mode-hook (lambda ()
                           ;; (local-set-key (kbd "M-.") nil)
                           ;; (local-set-key (kbd "M-,") nil)
                           (local-set-key (kbd "C-c r .") 'xref-find-definitions)
                           (local-set-key (kbd "C-c r ,") 'xref-find-references)
                           (define-key js2-mode-map (kbd "C-c r .") 'xref-find-definitions)
                           (define-key js2-mode-map (kbd "C-c r ,") 'xref-find-references)
                           (define-key js2-mode-map (kbd "C-c C-e") 'nodejs-repl-send-last-expression)
                           (define-key js2-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
                           (define-key js2-mode-map (kbd "C-c C-c") 'nodejs-repl-send-buffer)
                           (define-key js2-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
                           (define-key js2-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
                           (define-key js2-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)))


(provide 'jong-nodejs)
