(use-package go-mode
  :ensure t)

(use-package go-autocomplete
  :ensure t)

(use-package go-guru
  :ensure t)

(use-package godoctor
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


(defun jong-set-go-envs()
  "Set environment variables relative with go."
  (interactive)
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-copy-envs '("PATH" "GOROOT" "GOPATH"))))

(add-to-list 'exec-path (expand-file-name (format "%s/bin" (getenv "GOPATH"))))
(add-to-list 'exec-path (expand-file-name (format "%s/bin/godef" (getenv "GOPATH"))))


(add-to-list #'jong-kill-buffer-patterns "*jong-error*")
(add-to-list #'jong-kill-buffer-patterns "*go-guru-output*")
(add-to-list #'jong-kill-buffer-patterns "*Gofmt Errors*")

(defun jong-go-chan-gud-stepout ()
  "This is ..."
  (interactive)
  (let ((current-buffer-name (buffer-name))
        (gud-buffer-pattern "^\*gud-.*")
        (target-buffer nil)
        (temp-buffer-list (buffer-list)))
    ;; Current buffer is gud.
    (if (and (string-match gud-buffer-pattern current-buffer-name)
             (equal major-mode 'chan-gogud-mode))
        (setq target-buffer (current-buffer))
      (catch 'loop
        (dolist (buffer temp-buffer-list)
          (with-current-buffer buffer
            (when (and (string-match gud-buffer-pattern (buffer-name buffer))
                       (equal major-mode 'chan-gogud-mode))
              (setq target-buffer buffer)
              (message "im here!!!")
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
        (base-pos (point))
        (package-url-list nil)
        (import-start-pattern "^.*import.*[(]")
        (import-end-pattern ".*)")
        (extension (file-name-extension (buffer-file-name)))
        (buffer-temp nil)
        (command nil))
    (if (not (equal extension "go"))
        (progn
          (message "This file is not for golang...")
          nil)
      (progn
        (goto-char (point-min))
        (re-search-forward import-start-pattern)
        (set-mark (point))
        (re-search-forward import-end-pattern)
        (if (region-active-p)
            ;; (message-box "ext : %s sub %s" extension (buffer-substring (region-beginning) (1- (region-end))))
            (progn
              (setq buffer-temp (buffer-substring (region-beginning) (1- (region-end))))
              (setq package-url-list (split-string buffer-temp "\n"))
              (with-current-buffer (get-buffer-create output-buffer)
                (ignore-errors (shell (current-buffer)))
                (display-buffer (current-buffer))
                (dolist (package-url package-url-list)
                  ;; (setq package-url (string-trim package-url))
                  ;; (setq command (format "go get %s" package-url))
                  ;; (shell-command command (current-buffer))
                  ;; (start-process "go get" (current-buffer) "go" "get" package-url)
                  (start-process-shell-command "go-get" (current-buffer) (format "go get %s" package-url))
                  ;; (goto-char (point-max))
                  ;; (insert command)
                  ;; (eshell-send-input)
                  ;; (autopair-newline)
                  ))))
        (deactivate-mark)
        (goto-char base-pos)))
    )
  )


(defun jong-set-go-bins ()
  "Check if GOPATH environment variable is set or not.
And the environment variable was existing, Download go binaries from the internet..."
  (interactive)
  (let ((cmd nil)
        (buffer-error "*jong-error*")
        (list-url (list "github.com/golang/lint/golint"
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




(add-hook 'go-mode-hook (lambda ()
                          (setq-default indent-tabs-mode nil)
                          (setq-default tab-width 3)))

(setq gofmt-command "goimports")

(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook (lambda ()
                          (setq gofmt-command "goimports")
                          (if (not (string-match "go" compile-command))
                              (set (make-local-variable 'compile-command)
                                   "go build -v && go test -v && go vet"))))

(defun jong-debug-go-project ()
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

(defcustom jong-go-run-command nil
  "This is varialbe for project run"
  :type 'string)


(defun jong-go-set-project-run-command ()
  (interactive)
  (let ((command))
    (setq command (read-string "Enter the command : "))
    (setq jong-go-run-command command)
    (message "Next run command is : %s" jong-go-run-command)
    )
  )


(defun jong-go-run-project ()
  (interactive )
  (let ((output-buffer "*jong-output*")))
  (if jong-go-run-command
      (shell-command jong-go-run-command)
    (message "The command was not setted."))
  )


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
  (let ((target-port "")
        (target-buffer "gud-connect"))
    (if (get-buffer target-buffer)
        (kill-buffer target-buffer))
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

    (if (get-buffer output-buffer)
        (kill-buffer output-buffer))

    (if (equal (projectile-project-root) nil)
        (setq target-dir (projectile-project-root))
      (setq target-dir default-directory))
    
    ;; start headless delve
    (with-current-buffer (get-buffer-create output-buffer)
      (display-buffer output-buffer)
      (setq default-directory target-dir)
      (ignore-errors (term-mode))
      (start-process "dlv-server-debug" (current-buffer) "dlv" "debug" "--headless")
      (ignore-errors (term-mode)))
    )
  )


(defun chan-run-dlv-cs (&optional otherframe)
  "Create dlv with server and client mode."
  (interactive)
  (let ((port)
        (start-pos)
        (end-pos)
        (magic-seconds 20)
        (main-file "main.go")
        (log-frame "log-frame")
	     (input-frame "input-frame")
        (target-frame)
        (current-frame (selected-frame)))
    (catch 'exit
      (condition-case ex
          (progn
            ;; Run server dlv process.
            (with-current-buffer (get-buffer main-file)
              (chan-run-dlv-server))
            ;; Waiting a server process reveal.
            (setq port (with-current-buffer (get-buffer "*chan-dlv-server*")
		                   (while (< (length (buffer-string)) 1)
			                  (message "waiting the seconds : %d"
                                    (setq magic-seconds (1- magic-seconds)))
			                  (sleep-for 1)
                           (when (equal magic-seconds 0)
                             (throw 'exit magic-seconds)))
                         (goto-char (point-max))
                         (forward-line -1)
                         (end-of-line)
                         (setq end-pos (point))
                         (re-search-backward ":")
                         (setq start-pos (1+ (point)))
                         (buffer-substring start-pos end-pos)))
            ;; Run client dlv process.
            (with-current-buffer (get-buffer main-file)
              (chan-run-dlv-client port)))
        (message "There was not a main.go buffer."))
      (progn
        (message "Waiting time was gone...")
        nil))
    
    (when otherframe
      (if (setq target-frame
                (catch 'target
                  (dolist (frame (frame-list))
                    (if (equal log-frame (frame-parameter frame 'name))
                        (throw 'target frame)))))
          (progn
            (select-frame-set-input-focus target-frame)
            (switch-to-buffer "*chan-dlv-server*")
            (select-frame-set-input-focus current-frame)
            (switch-to-buffer-other-window main-file)
            (other-window 1)
            )
        (progn
          (setq target-frame (make-frame
                              '((name . "log-frame"))
                              ))
          (select-frame-set-input-focus target-frame)
          (switch-to-buffer "*chan-dlv-server*")
          (select-frame-set-input-focus current-frame)
          (switch-to-buffer-other-window main-file)
          (other-window 1)
          )))
    )
  )

(defun jong-run-dlv-cs-otherframe ()
  "Create dlv with server (other-frame) and client mode."
  (interactive)
  (chan-run-dlv-cs t)
  )


(add-hook 'go-mode-hook 'jong-go-set-gud-shortcut)
(add-hook 'go-mode-hook (lambda ()
                          (setq indent-tabs-mode nil)
                          (setq tab-width 4)
                          
                          ;; syntax highlight
                          (go-guru-hl-identifier-mode)
                          (go-eldoc-setup)
                          
                          ;; (require 'auto-complete-config)
                          ;; (ac-config-default)
                          
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
                          (local-set-key (kbd "C-c s f") 'gofmt-before-save)
                          (local-set-key (kbd "C-c g c") 'chan-run-dlv-cs)
                          (local-set-key (kbd "C-c c c")
                                         (lambda () (interactive)
                                           (compile "go build -v")))
                          ;; (compile "go build -v && go test -v && go vet")))
                          (local-set-key (kbd "C-c r r") 'jong-go-run-project)
                          (local-set-key (kbd "C-c r s") 'jong-go-set-project-run-command)
                          (local-set-key (kbd "C-c M->")
                                         (lambda () (interactive)
                                           (other-window 1)
                                           (call-interactively 'end-of-buffer)
                                           (other-window -1)))
                          )
          )


(add-hook 'chan-gogud-mode-hook 'jong-go-set-gud-shortcut)
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
