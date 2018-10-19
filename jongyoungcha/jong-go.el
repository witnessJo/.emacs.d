(use-package go-mode
  :ensure t)

(use-package go-guru
  :ensure t)

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

(use-package go-dlv
  :ensure t)


(defun jo-set-go-envs()
  "Set environment variables relative with go."
  (interactive)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-copy-envs '("PATH" "GOROOT" "GOPATH"))))

(add-to-list #'jo-kill-target-buffers "*jo-error*")
(defun jo-set-go-bins ()
  "Check if GOPATH environment variable is set or not.
And the environment variable was existing, Download go binaries from the internet..."
  (interactive)
  (let ((cmd nil)
        (buffer-error "*jo-error*")
        (list-url (list "github.com/golang/lint/golint"
                        "github.com/nsf/gocode"
                        "github.com/k0kubun/pp"
                        "github.com/golang/lint/golint"
                        "github.com/rogpeppe/godef"
                        "github.com/dougm/goflymake"
                        "golang.org/x/tools/cmd/godoc"
                        "golang.org/x/tools/cmd/guru")))
    (if (getenv "GOPATH")
        (progn
          (dolist (elt list-url cmd)
            (setq cmd (format "go get %s" elt))
            (when (shell-command cmd nil buffer-error)
              (with-current-buffer buffer-error (insert (format "(# command : %s)" cmd))))
            (set-window-buffer (get-buffer-window) buffer-error)))
      (message "There was not the GOPATH environment variable.")))
  )

(defun jo-set-projectile-run-command ()
  "Read user input command and set 'projectile-project-run-cmd'."
  (interactive)
  (let (user-input)
    (if (not (equal "" (setq user-input (read-string "Enter the command : "))))
        (progn
          (setq projectile-project-run-cmd user-input)
          (message "Changed projectile-project-run-cmd as %s" user-input))
      (message "The command was empty..."))
    ))


(add-to-list 'exec-path (expand-file-name "~/projects/goworks/bin/godef"))

;; (add-hook 'go-mode-hook (lambda ()
;;                           (setq-default indent-tabs-mode nil)
;;                           (setq-default tab-width 3)))

(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
                          (setq gofmt-command "goimports")
                          (if (not (string-match "go" compile-command))
                              (set (make-local-variable 'compile-command)
                                   "go build -v && go test -v && go vet"))))

(add-hook 'go-mode-hook (lambda ()
                          (setq indent-tabs-mode nil)
                          (setq tab-width 4)
                          
                          ;; syntax highlight
                          (go-guru-hl-identifier-mode)
                          
                          (go-eldoc-setup)
                          (add-hook 'before-save-hook 'gofmt-before-save)

                          ;; setting company-go mode...
                          (setq company-tooltip-limit 20)
                          (setq company-idle-delay .3)
                          (setq company-echo-delay 0)
                          (setq company-begin-commands '(self-insert-command))
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)

                          ;;setting go-eldocp
                          (set-face-attribute 'eldoc-highlight-function-argument nil
                                              :underline t :foreground "green"
                                              :weight 'bold)
                          
                          (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
                          (local-set-key (kbd "C-c C-a") 'go-import-add)
                          (local-set-key (kbd "C-c C-g") 'go-goto-imports)
                          (local-set-key (kbd "C-c C-f") 'gofmt)
                          (local-set-key (kbd "C-c r .") 'godef-jump)
                          (local-set-key (kbd "C-c d d") 'godoc-at-point)
                          (local-set-key (kbd "C-c c c")
                                         (lambda () (interactive)
                                           (compile "go build -v && go test -v && go vet")))
                          (local-set-key (kbd "C-c r s") 'jo-set-projectile-run-command)
                          (local-set-key (kbd "C-c r r") 'projectile-run-project)))


(provide 'jong-go)
