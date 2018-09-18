(use-package js2-mode
  :ensure t)

(use-package js2-refactor
  :ensure t)

(use-package xref-js2
  :ensure t)

;; (use-package nodejs-repl
;;   :ensure t)

(use-package indium
  :ensure t)

(use-package tern
  :ensure t)
(eval-after-load "tern"
  '(progn
     (define-key tern-mode-keymap (kbd "M-.") nil)
     (define-key tern-mode-keymap (kbd "M-,") nil)
     (define-key tern-mode-keymap (kbd "C-c C-b") nil)
     (define-key tern-mode-keymap (kbd "C-c C-r") nil)))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))


(use-package company-tern
  :ensure t)
(add-to-list 'company-backends 'company-tern)

(add-hook 'js2-mode-hook #'tern)
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(add-hook 'js2-mode-hook #'indium-interaction-mode)

(add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))

(add-hook 'js2-mode-hook (lambda ()

                           (local-set-key (kbd "C-c r .") 'xref-find-definitions)
                           (local-set-key (kbd "C-c r ,") 'xref-find-references)
                           (define-key js2-mode-map (kbd "C-c r .") 'xref-find-definitions)
                           (define-key js2-mode-map (kbd "C-c r ,") 'xref-find-references)
                           (define-key js2-mode-map (kbd "C-c r l") 'indium-eval-last-node)
                           (define-key js2-mode-map (kbd "C-c r r") 'indium-eval-region)
                           (define-key js2-mode-map (kbd "C-c r b") 'indium-eval-buffer)
                           (define-key js2-mode-map (kbd "C-c :") 'indium-inspect-expression)))


(provide 'jong-nodejs)
