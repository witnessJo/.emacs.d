;;; Code:

(defvar jong-go-debug-buffer "*jong-go-debug*" "Jong go language debug buffer.")

(use-package go-mode
	:ensure t)

(use-package go-guru
	:ensure t)

(use-package company-go
	:ensure t)

(use-package go-eldoc
	:ensure t
	:config
	(add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-complete
	:ensure t
	:config
	(add-hook 'completion-at-point 'go-complete-at-point))

(use-package flycheck
	:ensure t)

(use-package go-dlv
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
				(list-url (list "github.com/golang/lint/golint"
												"github.com/mdempsky/gocode"
												"github.com/k0kubun/pp"
												"github.com/golang/lint/golint"
												"github.com/rogpeppe/godef"
												"github.com/dougm/goflymake"
												"golang.org/x/tools/cmd/vet"
												"golang.org/x/tools/cmd/godoc"
												"golang.org/x/tools/cmd/guru"
												"golang.org/x/tools/cmd/goimports"
												"golang.org/x/tools/cmd/gopls@lastest"
												"github.com/go-delve/delve/cmd/dlv")))
		(if (getenv "GOPATH")
				(progn
					(dolist (elt list-url cmd)
						(setq cmd (format "go get -u %s" elt))
						(with-current-buffer (get-buffer-create buffer-name)
							(shell-command cmd (current-buffer) (current-buffer)))))
			(message "There was not the GOPATH environment variable."))
		)
	)



(setq gofmt-command "goimports")

(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook (lambda ()
													(setq gofmt-command "goimports")
													(if (not (string-match "go" compile-command))
															(set (make-local-variable 'compile-command)
																	 "go build -v && go test -v && go vet"))))

(defun jong-go-debug-project ()
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
	"This is varialbe for project run."
	:type 'string)

(defcustom jong-go-run-default-path nil
	"This is varialbe for project default directory."
	:type 'string)

(defun jong-go-set-project-run-command ()
	(interactive)
	(let ((command))
		(setq command (read-string "Enter the command : "))
		(setq jong-go-run-command command)
		(setq jong-go-run-default-path default-directory)
		(message "Next run command : [%s], default path : [%s]"
						 jong-go-run-command jong-go-run-default-path)
		)
	)

(defun jong-go-run-project ()
	(interactive )
	(let ((output-buffer-name "*jong-output*")
				(output-buffer nil)
				(program-name nil)
				(program-args nil))
		(ignore-errors (kill-buffer output-buffer-name))
		(with-current-buffer (get-buffer-create output-buffer-name)
			(if jong-go-run-command
					(progn
						(display-buffer (current-buffer))
						(setq default-directory jong-go-run-default-path)
						(async-shell-command jong-go-run-command (current-buffer) (current-buffer)))
				(start-process jong-go-run-command (current-buffer) program-name program-args))
			(message "The command was not setted.")))
	)

(defun jong-go-run-project-otherframe ()
	(interactive)
	(let ((current-frame (selected-frame))
				(output-buffer-name "*jong-output*")
				(output-frame-name "log-frame")
				(output-buffer nil)
				(output-frame nil)
				(program-name nil)
				(program-args nil))

		(ignore-errors (kill-buffer output-buffer-name))
		(setq output-buffer (get-buffer-create output-buffer-name))
		(if (setq output-frame

							(catch 'found
								(dolist (frame (frame-list))
									(if (equal output-frame-name (frame-parameter frame 'name))
											(throw 'found frame)))))
				(progn
					(select-frame-set-input-focus output-frame)
					(switch-to-buffer output-buffer-name))
			(progn
				(setq output-frame (make-frame
														'((name . "log-frame"))
														))
				(select-frame-set-input-focus output-frame)
				(switch-to-buffer output-buffer-name)))
		(with-current-buffer (get-buffer output-buffer)
			(if jong-go-run-command
					(progn
						(setq default-directory jong-go-run-default-path)
						(async-shell-command jong-go-run-command (current-buffer) (current-buffer)))
				(message "go-run-command was not setted...")))
		(select-frame-set-input-focus current-frame)
		)
	)


(define-derived-mode jong-gogud-mode gud-mode "jong-gogud"
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


(defun jong-gogud-gdb (&optional cmd)
	"This is delve wrapper based on 'gud-gdb mode."
	(interactive)
	(let ((cmd))
		(with-current-buffer (current-buffer)
			(setq cmd (read-string "dlv command :" "dlv exec"))
			(when (equal cmd nil)
				(setq cmd "dlv debug"))
			(dlv cmd)
			(jong-gogud-mode)
			))
	)


(defun chan-run-dlv-client(&optional port)
	"Connect the dlv server!!!."
	(interactive)
	(let ((target-port "")
				(output-buffer "*gud-connect*")
				(process-name nil)
				(waiting-seconds 0))

		(when (get-buffer output-buffer)
			(with-current-buffer (get-buffer output-buffer)
				(while (get-buffer-process (current-buffer))
					(comint-send-eof)
					(message "waiting killing the gud-connect process. (%d seconds)" waiting-seconds)
					(sleep-for 1)
					(1+ waiting-seconds))

				(kill-buffer (current-buffer))))

		(if (equal port nil)
				(setq target-port (read-string "input listen port : "))
			(setq target-port port))
		(dlv (format "dlv connect :%s" target-port))
		(jong-gogud-mode))
	)


(defun chan-run-dlv-server()
	"Make run interactively!!!."
	(interactive)
	(let ((target-dir nil)
				(output-buffer "*chan-dlv-server*")
				(process-name nil)
				(listen-process nil))

		(if (get-buffer output-buffer)
				(kill-buffer output-buffer))

		(if (equal (projectile-project-root) nil)
				(setq target-dir (projectile-project-root))
			(setq target-dir default-directory))

		;; start headless delve
		(with-current-buffer (get-buffer-create output-buffer)
			(when (get-buffer-process (current-buffer))
				(interrupt-process process-name)
				(while (get-buffer-process (current-buffer))
					(message "Killing process : %s " process-name)
					(sleep-for 1))
				(comint-clear-buffer))

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
	(chan-run-dlv-cs t))


;; (add-hook 'go-mode-hook 'jong-go-set-gud-shortcut)
(add-hook 'go-mode-hook (lambda ()
													(setq lsp-ui-sideline-enable nil)
													(setq lsp-ui-doc-enable nil)
													(setq lsp-gopls-staticcheck t)
													(setq lsp-eldoc-render-all t)
													(setq lsp-gopls-complete-unimported t)
													(lsp)
													
													(setq indent-tabs-mode t)
													(setq tab-width 4)

													;; syntax highlight
													(go-guru-hl-identifier-mode)

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
													(local-set-key (kbd "C-c r w") 'lsp-workspace-restart)
													(local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
													(local-set-key (kbd "C-c C-a") 'go-import-add)
													(local-set-key (kbd "C-c C-g") 'go-goto-imports)
													(local-set-key (kbd "C-c C-f") 'gofmt)
													(local-set-key (kbd "C-c r .") 'lsp-find-definition)
													(local-set-key (kbd "C-c r ,") 'lsp-find-references)
													(local-set-key (kbd "C-c r i") 'lsp-find-implementation)
													(local-set-key (kbd "C-c r j") 'go-guru-definition)
													(local-set-key (kbd "C-c r d") 'go-guru-describe)
													(local-set-key (kbd "C-c o i") 'lsp-organize-imports)
													(local-set-key (kbd "C-c r l") 'helm-imenu)
													(local-set-key (kbd "C-c g g")
																				 (lambda () (interactive)
																					 (jong-gogud-gdb "dlv debug")))
													(local-set-key (kbd "C-c g i") 'jong-get-imported-packages)
													(local-set-key (kbd "C-c s f") 'gofmt-before-save)
													(local-set-key (kbd "C-c g c") 'chan-run-dlv-cs)
													(local-set-key (kbd "C-c c c") 'jong-project-compile-project)
													;; (compile "go build -v && go test -v && go vet")))
													(local-set-key (kbd "C-c r r") 'lsp-rename)
													;; (local-set-key (kbd "C-c r r") 'jong-go-run-project-otherframe)
													(local-set-key (kbd "C-c r s") 'jong-go-set-project-run-command)
													(local-set-key (kbd "C-c M->")
																				 (lambda () (interactive)
																					 (other-window 1)
																					 (call-interactively 'end-of-buffer)
																					 (other-window -1)))
													)
					)


;; (add-hook 'chan-gogud-mode-hook 'jong-go-set-gud-shortcut)
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
						))


(provide 'jong-go)
;;;
