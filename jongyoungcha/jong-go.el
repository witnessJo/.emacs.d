(use-package go-mode
  :ensure t)
;; (use-package go-flymake
;;   :ensure t)
;; (use-package go-flycheck
;;   :ensure t)
(use-package direx
  :ensure t)

(use-package popwin
  :ensure t)

(use-package company-go
  :ensure t)

(use-package go-eldoc
  :ensure t)

(use-package go-direx
  :ensure t)

(use-package go-eldoc
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package flymake-go
  :ensure t)

(use-package go-stacktracer
  :ensure t)

(use-package helm-go-package
  :ensure t)

(use-package go-errcheck
  :ensure t)

(add-to-list 'exec-path (expand-file-name "~/projects/goworks/bin/godef"))

(add-hook 'go-mode-hook (lambda ()
                          (go-eldoc-setup)
                          (add-hook 'before-save-hook 'gofmt-before-save)
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
                          (local-set-key (kbd "C-c C-a") 'go-import-add)
                          (local-set-key (kbd "C-c C-g") 'go-goto-imports)
                          (local-set-key (kbd "C-c C-f") 'gofmt)
                          (local-set-key (kbd "C-c r .") 'godef-jump)))





(provide 'jong-go)
