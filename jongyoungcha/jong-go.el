 ;;; Code:

(defvar jong-go-debug-buffer "*jong-go-debug*" "Jong go language debug buffer.")

(use-package go-mode
  :ensure t)

(use-package company-go
  :ensure t)

(use-package gotest
  :ensure t
  :config
  (setq go-test-verbose t)
  (add-hook 'go-test-mode-hook (lambda()
								(visual-line-mode)))
  )

(use-package go-tag
  :ensure t
  :config
  (setq go-tag-args (list "-transform" "snakecase"))
  )

(use-package go-fill-struct
  :ensure t)

(exec-path-from-shell-getenv "GOPATH")
(exec-path-from-shell-getenv "GOROOT")
(exec-path-from-shell-getenv "PATH")

(defun jong-set-go-envs()
  "Set environment variables relative with go."
  (interactive)
  (when (memq window-system '(mac ns x))
	(exec-path-from-shell-copy-envs '("PATH" "GOROOT" "GOPATH")))
  )

(if (getenv "GOPATH")
	(add-to-list 'exec-path (expand-file-name (format "%s/bin" (getenv "GOPATH"))))
  (error "$GOPATH was not exported"))

(add-to-list  #'jong-kill-buffer-patterns "*jong-error*")
(add-to-list  #'jong-kill-buffer-patterns "*go-guru-output*")
(add-to-list  #'jong-kill-buffer-patterns "*Gofmt Errors*")

(defun jong-go-chan-gud-stepout ()
  "This is ..."
  (interactive)
  (let ((current-buffer-name (buffer-name))
		(gud-buffer-pattern "^\*gud-.*")
		(target-buffer nil)
		(temp-buffer-list (buffer-list)))
	;; Current buffer is gud.
	(if (and (string-match gud-buffer-pattern current-buffer-name)
			 (equal major-mode 'jong-gogud-mode))
		(setq target-buffer (current-buffer))
	  (catch 'loop
		(dolist (buffer temp-buffer-list)
		  (with-current-buffer buffer
			(when (and (string-match gud-buffer-pattern (buffer-name buffer))
					   (equal major-mode 'jong-gogud-mode))
			  (setq target-buffer buffer)
			  (throw 'loop buffer))))))

	(when target-buffer
	  (with-current-buffer target-buffer
		(goto-char (point-max))
		(send-string (get-buffer-process (current-buffer)) "stepout\n")))
	))


(defun jong-go-set-gud-shortcut ()
  "Set shortcuts of gud for golang."
  
  (local-set-key (kbd "<f7>") (lambda () (interactive)
								(call-interactively 'gud-print)
								(call-interactively 'end-of-buffer)))

  (local-set-key (kbd "<f8>") (lambda () (interactive)
								(call-interactively 'gud-cont)))

  (local-set-key (kbd "<f9>") (lambda () (interactive)
								(call-interactively 'gud-break)))

  (local-set-key (kbd "<f10>") (lambda () (interactive)
								 (call-interactively 'gud-next)
								 (call-interactively 'end-of-buffer)))

  (local-set-key (kbd "<f11>") (lambda () (interactive)
								 (call-interactively 'gud-step)
								 (call-interactively 'end-of-buffer)))

  (local-set-key (kbd "<f12>") 'jong-go-chan-gud-stepout))


(defun jong-get-imported-packages ()
  "Get Imported package "
  (interactive)
  (let ((output-buffer "*jong-output-buffer*")
		(extract-pattern-whole "^[[:space:]]*import[[:space:]]*(\\([[:ascii:]]+?\\))")
		(extract-pattern-elem "^[[:space:]]*\\([[:alpha:]]+\\)[[:space:]]*\\([\\\s_\\\s-\\\s\"\\\s.\\\s/[[:alpha:]]*]*\\)")
		(imported-string "")
		(base-pos (point))
		(package-url-list nil)
		(import-start-pattern "^.*import.*[(]")
		(import-end-pattern ".*)")
		(extension (file-name-extension (buffer-file-name)))
		(buffer-temp nil)
		(command nil)
		(imported-packages))
	(unless (equal extension "go")
	  (error "This file is not for golang"))

	(string-match extract-pattern-whole (buffer-substring-no-properties (point-min) (point-max)))
	(setq imported-string (match-string 1 (buffer-substring-no-properties (point-min) (point-max))))
	(setq imported-packages (split-string imported-string "\n"))
	(with-current-buffer (get-buffer-create output-buffer)
	  (progn
		(dolist (package-uri imported-packages)
		  (if (string-match extract-pattern-elem package-uri)
			  (progn
				(when (match-string 1 package-uri)
				  (insert (format "%s\n" (match-string 1 package-uri)))
				  (start-process-shell-command "go"
											   (current-buffer)
											   (format "go get %s" (match-string 1 package-uri))))
				(when (match-string 2 package-uri)
				  (insert (format "%s\n" (match-string 2 package-uri)))
				  (start-process-shell-command "go"
											   (current-buffer)
											   (format "go get %s" (match-string 2 package-uri)))))
			(start-process-shell-command "go"
										 (current-buffer)
										 (format "go get %s" package-uri)))
		  )
		)
	  )
	)
  )


(defun jong-set-go-bins ()
  "Check if GOPATH environment variable is set or not.
And the environment variable was existing, Download go binaries from the internet..."
  (interactive)
  (let ((cmd nil)
		(buffer-name "*jong-set-go-bins*")
		(list-url (list "golang.org/x/tools/gopls@latest"
						"github.com/golang/lint/golint"
						"github.com/mdempsky/gocode"
						"github.com/k0kubun/pp"
						"github.com/rogpeppe/godef"
						"github.com/dougm/goflymake"
						"golang.org/x/tools/cmd/vet"
						"golang.org/x/tools/cmd/godoc"
						"golang.org/x/tools/cmd/guru"
						"golang.org/x/tools/cmd/goimports"
						"github.com/go-delve/delve/cmd/dlv"
						"github.com/fatih/gomodifytags")))
	(if (getenv "GOPATH")
		(progn
		  (dolist (elt list-url cmd)
			;; (setq cmd (format "go get -u %s" elt))
			(with-current-buffer (get-buffer-create buffer-name)
			  (display-buffer (current-buffer))
			  (ignore-errors (call-process "go" nil t 0 "get" "-u" elt)))))
	  (message "There was not the GOPATH environment variable."))
	)
  )

(add-hook 'go-mode-hook #'lsp-deferred)

(defun lsp-go-install-save-hooks ()
  "Save configuration before save."
  ;; (add-hook 'before-save-hook #'lsp-format-buffer t t)
  ;; (add-hook 'before-save-hook #'lsp-organize-imports t t)
  )

;; (add-hook 'go-mode-hook
;; (lambda()
;; (add-hook 'before-save-lsp
;; (lambda()
;; (call-interactively 'lsp)))))

(add-hook 'go-mode-hook (lambda ()
						  (setq go-test-args "-count=1")
						  (setq lsp-ui-sideline-enable t)
						  (setq lsp-gopls-staticcheck t)
						  (setq lsp-ui-doc-enable nil)
						  (setq lsp-ui-doc-position 'at-point)
						  ;; (setq lsp-ui-doc-delay 0.5)
						  (setq lsp-gopls-complete-unimported t)
						  
						  (setq indent-tabs-mode t)

						  ;; setting company-go mode...
						  (setq company-tooltip-limit 20)
						  (setq company-idle-delay .3)
						  (setq company-echo-delay 0)
						  (setq company-begin-commands '(self-insert-command))
						  (set (make-local-variable 'company-backends) '(company-go))
						  (company-mode)

						  ;; :weight 'bold)
						  (local-set-key (kbd "C-c k") 'lsp-ui-doc-show)
						  (local-set-key (kbd "C-c r w") 'lsp-workspace-restart)
						  (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
						  (local-set-key (kbd "C-c C-a") 'go-import-add)
						  (local-set-key (kbd "C-c C-g") 'go-goto-imports)
						  (local-set-key (kbd "C-c C-f") 'gofmt)
						  (local-set-key (kbd "C-c C-c") 'lsp-execute-code-action)
						  (local-set-key (kbd "M-.") 'lsp-ui-peek-find-definitions)
						  (local-set-key (kbd "C-c r ,") 'lsp-ui-peek-find-references)
						  (local-set-key (kbd "C-c r i") 'lsp-ui-peek-find-implementation)
						  (local-set-key (kbd "C-c o i") 'lsp-ui-organize-imports)
						  (local-set-key (kbd "C-c r l") 'helm-imenu)
						  ;; (local-set-key (kbd "C-c g i") 'jong-get-imported-packages)
						  (local-set-key (kbd "C-c s f") 'gofmt-before-save)
						  ;; (local-set-key (kbd "C-c g c") 'chan-run-dlv-cs)
						  (local-set-key (kbd "C-c c c") 'jong-project-compile-project)
						  (local-set-key (kbd "C-c r r") 'lsp-rename)
						  
						  (local-set-key (kbd "C-c t f") 'go-test-current-test)
						  (local-set-key (kbd "C-c t a") 'go-test-current-file)
						  (local-set-key (kbd "C-c t p") 'go-test-current-test-cache)
						  (local-set-key (kbd "C-c r s") 'jong-go-set-project-run-command)
						  ;; (local-set-key (kbd "C-c M->")
						  ;; (lambda () (interactive)
						  ;; (other-window 1)
						  ;; (call-interactively 'end-of-buffer)
						  ;; (other-window -1)))
						  (lsp)

						  ;; disable eldoc mode
						  ;; (setq eldoc-mode nil)
						  (eldoc-mode)
						  )
		  )

(provide 'jong-go)
;;;
