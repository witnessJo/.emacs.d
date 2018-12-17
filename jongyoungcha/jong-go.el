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
(add-to-list #'jo-kill-target-buffers "*go-guru-output*")
(add-to-list #'jo-kill-target-buffers "*Gofmt Errors*")

(defun jo-set-go-bins ()
  "Check if GOPATH environment variable is set or not.
And the environment variable was existing, Download go binaries from the internet..."
  (interactive)
  (let ((cmd nil)
        (buffer-error "*jo-error*")
        (list-url (list "github.com/golang/lint/golint"
                        "github.com/nsf/gocode"
                        "github.com/mdempsky/gocode"
                        "github.com/k0kubun/pp"
                        "github.com/golang/lint/golint"
                        "github.com/rogpeppe/godef"
                        "github.com/dougm/goflymake"
                        "golang.org/x/tools/cmd/godoc"
                        "golang.org/x/tools/cmd/guru"
                        "golang.org/x/tools/cmd/goimports")))
    (if (getenv "GOPATH")
        (progn
          (dolist (elt list-url cmd)
            (setq cmd (format "go get %s" elt))
            (when (shell-command cmd nil buffer-error)
              (with-current-buffer buffer-error (insert (format "(# command : %s)" cmd))))
            (set-window-buffer (get-buffer-window) buffer-error)))
      (message "There was not the GOPATH environment variable.")))
  )

;; (defun jo-set-projectile-run-command ()
;;   "Read user input command and set 'projectile-project-run-cmd'."
;;   (interactive)
;;   (let (user-input)
;;     (if (not (equal "" (setq user-input (read-string "Enter the command : "))))
;;         (progn
;;           (setq projectile-project-run-cmd user-input)
;;           (message "Changed projectile-project-run-cmd as %s" user-input))
;;       (message "The command was empty..."))
;;     ))


(add-to-list 'exec-path (expand-file-name "~/goworks/bin/godef"))
(add-to-list 'exec-path (expand-file-name "~/goworks/bin"))


(add-hook 'go-mode-hook (lambda ()
                          (setq-default indent-tabs-mode nil)
                          (setq-default tab-width 3)))

;; (add-hook 'completion-at-point-functions 'go-complete-at-point)

(setq gofmt-command "goimports")
;; (add-hook 'before-save-hook 'gofmt-before-save)


(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
                          (setq gofmt-command "goimports")
                          (if (not (string-match "go" compile-command))
                              (set (make-local-variable 'compile-command)
                                   "go build -v && go test -v && go vet"))))

(defun jo-debug-go-project ()
  "Debug the go project with delve."
  (interactive)
  (let ((cmd nil)
        (homedir nil))
    (setq homedir (projectile-project-root))
    (if homedir
        (with-temp-buffer
          (cd homedir)
          (call-interactively 'dlv))
      (message "Couldn't found the projectile root directory."))
    ))


(defun jo-projectile-run-project (&optional prompt)
  (interactive "P")
  (let ((compilation-read-command
         (or (not (projectile-run-command (projectile-compilation-dir)))
             prompt)))
    (projectile-run-project prompt)))


(define-derived-mode chan-gogud-mode gud-mode "chan-gogud"
  (setq font-lock-defaults '(go--build-font-lock-keywords)))


(defun chan-gogud-exec-function (target-func)
  "..."
  (interactive)
  (let ((base-line 0)
        (target-line 0)
        (current-line-buffer "")
        (target-symbol "")
        (target-offset 0))
    
    ;; Initailize other buffer cursor position...
    (gud-refresh)

    ;; Get Initial variables...
    (setq target-line (line-number-at-pos))
    (setq target-symbol (thing-at-point 'symbol))
    (if (equal target-symbol nil)
        (progn
          (message "Target symbol was nil...")
          (return)))

    ;; Get current line buffer...
    (setq current-line-buffer (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position)))

    ;; Calculates what times symbol was shown from the line...
    (setq target-offset
          (- (- (- (point) (line-beginning-position))
                (string-match ":" current-line-buffer)) 2))
    
    
    ;; Get base-line from the gud buffer.
    (goto-char (point-max))
    (while (not (string-prefix-p "=>" (current-line-contents)))
      (forward-line -1)
      (if (equal (point) 0)
          (progn
            (message "Couldnt find the '=>' prefix...")
            (return))))
    
    (setq base-line (line-number-at-pos))
    
    ;; Move other window and move the point to the target symbol.
    (other-window 1)
    (forward-line (- target-line base-line))
    (line-beginning-position)
    (goto-char (+ (point) target-offset))
    (call-interactively target-func)
    (other-window 1)
    (with-no-warnings
      (goto-line target-line))
    )
  )


(defun chan-gogud-gdb (&optional cmd)
  "This is delve wrapper based on 'gud-gdb mode."
  (interactive)
  (if (equal cmd nil)
      (setq cmd "dlv debug"))
  (condition-case ex
      (with-current-buffer (get-buffer "main.go")
        (dlv cmd)
        (chan-gogud-mode))
    (message "There was not a main.go buffer."))
  )


(defun chan-run-dlv-client(&optional port)
  "Connect the dlv server!!!."
  (interactive)
  (let ((target-port ""))
    (if (equal port nil)
        (setq target-port (read-string "input listen port : "))
      (setq target-port port))
    (dlv (format "dlv connect :%s" target-port))
    (chan-gogud-mode))
  )


(defun chan-run-dlv-server()
  "Make run interactively!!!."
  (interactive)
  (let ((target-dir nil)
        (output-buffer "*chan-dlv-server*")
        (listen-process nil))
    ;; (target-port nil)

    (if (equal (projectile-project-root) nil)
        (setq target-dir (projectile-project-root))
      (setq target-dir default-directory))
    
    ;; start headless delve
    (with-current-buffer (get-buffer-create output-buffer)
      (display-buffer output-buffer)
      (setq default-directory target-dir)
      (ignore-errors (async-shell-command "dlv debug --headless" (current-buffer) "*jo-error*"))
      ))
  )


(defun chan-run-dlv-cs ()
  "Create dlv with server and client mode."
  (interactive)
  (let ((port)
        (start-pos)
        (end-pos)
        (magic-seconds 0))
    (condition-case ex
        (progn
          (with-current-buffer (get-buffer "main.go")
            (chan-run-dlv-server))

          ;; (while (> (1+ magic-seconds) 4)
          ;; (message "waiting...")
          (sleep-for 4)
          
          (setq port (with-current-buffer (get-buffer-create "*chan-dlv-server*")
                       (goto-char (point-max))
                       (forward-line -1)
                       (end-of-line)
                       (setq end-pos (point))
                       (re-search-backward ":")
                       (setq start-pos (1+ (point)))
                       (buffer-substring start-pos end-pos)))
          (with-current-buffer (get-buffer "main.go")
            (chan-run-dlv-client port)))
      (message "There was not a main.go buffer.")))
  )


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
                          (local-set-key (kbd "C-c r ,") 'go-guru-referrers)
                          (local-set-key (kbd "C-c r i") 'go-guru-implements)
                          (local-set-key (kbd "C-c r j") 'go-guru-definition)
                          (local-set-key (kbd "C-c r d") 'go-guru-describe)
                          (local-set-key (kbd "C-c d d") 'godoc-at-point)
                          (local-set-key (kbd "C-c g g")
                                         (lambda () (interactive)
                                           (chan-gogud-gdb "dlv debug")))
                          (local-set-key (kbd "C-c g c") 'chan-run-dlv-cs)
                          (local-set-key (kbd "C-c c c")
                                         (lambda () (interactive)
                                           (compile "go build -v && go test -v && go vet")))
                          (local-set-key (kbd "C-c r r") 'jo-projectile-run-project)
                          (local-set-key (kbd "C-c M->")
                                         (lambda () (interactive)
                                           (other-window 1)
                                           (call-interactively 'end-of-buffer)
                                           (other-window -1)))
                          )
          )



(add-hook 'chan-gogud-mode-hook
          (lambda () (local-set-key (kbd "C-c r .")
                                    (lambda () (interactive)
                                      (call-interactively 'gud-refresh)
                                      (chan-gogud-exec-function #'godef-jump)))
            (local-set-key (kbd "C-c r ,")
                           (lambda () (interactive)
                             (call-interactively 'gud-refresh)
                             (chan-gogud-exec-function #'go-guru-referrers)))
            (local-set-key (kbd "C-c r i")
                           (lambda () (interactive)
                             (call-interactively 'gud-refresh)
                             (chan-gogud-exec-function #'go-guru-implements)))
            )
          )


(provide 'jong-go)
;;;
