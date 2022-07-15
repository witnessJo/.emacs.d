(require 'comint)

;;; Code:

(defconst jong-project-output-buffer "*jong-project-output*")
(defconst jong-project-debug-buffer "*jong-project-debug*")

(add-to-list 'jong-kill-buffer-patterns jong-project-output-buffer)
(add-to-list 'jong-kill-buffer-patterns jong-project-debug-buffer)

(defvar-local jong-project-compile-default-dir nil)
(defvar-local jong-project-compile-command nil)
(defvar-local jong-project-run-default-dir nil)
(defvar-local jong-project-run-command nil)
(defvar-local jong-project-debug-default-dir nil)
(defvar-local jong-project-debug-command nil)
(defvar-local jong-project-sub-default-dir-2 nil)
(defvar-local jong-project-sub-command-2 nil)
(defvar-local jong-project-sub-default-dir-3 nil)
(defvar-local jong-project-sub-command-3 nil)
(defvar-local jong-project-subcmds nil)

(put 'jong-project-compile-default-dir 'safe-local-variable #'stringp)
(put 'jong-project-compile-command 'safe-local-variable #'stringp)
(put 'jong-project-run-default-dir 'safe-local-variable #'stringp)
(put 'jong-project-run-command 'safe-local-variable #'stringp)
(put 'jong-project-debug-default-dir 'safe-local-variable #'stringp)
(put 'jong-project-debug-command 'safe-local-variable #'stringp)

(setq enable-local-variables t)
(setq enable-local-eval t)


(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

(defun jong-project-send-command-to-buffer (cmd &optional buffer)
  "Send string to the sub process"
  (let ((target-buffer nil))
	(if (equal buffer nil)
		(setq target-buffer buffer)
	  (setq target-buffer (current-buffer)))
	(with-current-buffer target-buffer
	  (comint-send-input cmd))
	)
  )


(defun jong-project-walkup-and-find-file (FILENAME &optional DIRECTORY)
  "Walkup and find a file .
'FILENAME' is a file name to fine
'DIRECTORY' is a base directory name'"
  (let ((path-to-find)
		(parent-dir))

	(when (equal DIRECTORY "/")
	  (error (format "Coudlnt find the target file (\"%s\") (Last directory was \"/\")" FILENAME)))

	(if (equal DIRECTORY nil)
		(setq DIRECTORY default-directory))

	(setq path-to-find (format "%s%s" DIRECTORY FILENAME))
	(if (file-exists-p path-to-find)
		path-to-find
	  (progn
		(setq parent-dir (file-name-directory (directory-file-name DIRECTORY)))
		(jong-project-walkup-and-find-file FILENAME parent-dir))
	  )
	)
  )


(defun jong-project-walkup-and-find-dot-dir-locals-el()
  (interactive)
  (let ((file-name ".dir-locals.el")
		(file-path))
	(setq file-path (jong-project-walkup-and-find-file file-name))
	(if file-path
		(find-file file-path)
	  (message "Couldnt find the \".dir-locals.el\""))
	)
  )


(defun jong-project-walkup-and-find-projectile-project-root ()
  (interactive)
  "Find .projectile and return project-root-path for projectile."
  (let ((dot-projectile ".projectile")
		(project-path))
	(setq project-path (jong-project-walkup-and-find-file dot-projectile))
	(if (equal project-path nil)
		(progn
		  (message "Couldnt find a dot-projectile file.")
		  "")
	  (file-name-directory (directory-file-name project-path)))
	)
  )


(defun jong-project-make-dot-dir-locals-el ()
  (interactive)
  (let ((target-directory)
		(target-path)
		(template))
	(setq target-directory (jong-project-walkup-and-find-projectile-project-root))
	(unless (file-exists-p target-directory)
	  (error "Couldnt find \".projectile\" file"))

	(setq target-path (format "%s%s" target-directory ".dir-locals.el"))
	(unless (file-exists-p target-path)
	  (setq template
			(concat "(\n"
					(concat
					 "(nil . (\n")
					(format "(jong-project-compile-default-dir . \"%s\")\n" target-directory)
					(format "(jong-project-compile-command . \"none\")\n")
					(format "(jong-project-run-default-dir . \"%s\")\n" target-directory)
					(format "(jong-project-run-command . \"none\")\n")
					(format "(jong-project-debug-default-dir . \"%s\")\n" target-directory)
					(format "(jong-project-debug-command . \"none\")\n")
					(format "(jong-project-sub-default-dir-2 . \"%s\")\n" target-directory)
					(format "(jong-project-sub-command-2 . \"none\")\n")
					(format "(jong-project-sub-default-dir-3 . \"%s\")\n" target-directory)
					"(eval . (progn\n"
					"(setq jong-project-subcmds\n"
					"(list\n"
					(format "(list \"testcmd1\" \"ls\" \"%s\")))))\n" target-directory)
					")\n"
					")\n"
					")\n"
					))
	  (write-region template nil target-path))
	(find-file target-path)
	)
  )


(defun jong-project-visit-dot-dir-locals-el ()
  (interactive)
  (let ((target-directory)
		(target-path))

	(setq target-directory (jong-project-walkup-and-find-projectile-project-root))
	(unless (file-exists-p target-directory)
	  (error "Couldnt find \".projectile\" file"))
	(setq target-path (format "%s%s" target-directory ".dir-locals.el"))
	(find-file target-path)
	)
  )

(defun jong-project-exec-command (directory cmd &optional after-func)
  (interactive)
  (let ((proc)
        (default-directory (if (not (file-directory-p directory))
							   (error "The directory was not existing")
							 directory))
		(buffer-name (format "*jong-command-%s-%s*" directory cmd)))
	(unless (file-directory-p directory)
	  (error (format "Couldnt find the directory %s\"" directory)))

	(when (or (equal cmd nil) (string= cmd ""))
	  (error (format "Couldnt find  %s\"" cmd)))
	(when (get-buffer buffer-name) (kill-buffer buffer-name))
	(with-current-buffer (get-buffer-create buffer-name)
	  ;; (compilation-modet)
	  (ansi-color-for-comint-mode-on)
	  (comint-mode)
	  (setq default-directory directory)
	  (display-buffer (current-buffer))
	  (setq proc (start-process-shell-command
                  buffer-name
                  (current-buffer)
                  cmd))
	  (set-process-filter proc 'comint-output-filter)
      )
    
    ;; (setq proc (start-file-process-shell-command buffer-name buffer-name cmd))
    ;; (set-process-filter proc 'comint-output-filter)
	;; (set-process-sentinel
    ;; proc
	;; (lambda (p e)
    ;; After process done
    ;; (with-current-buffer (get-buffer (process-buffer p))
	;; (compilation-mode)
	;; (compilation-shell-minor-mode)
    ;; )
    ;; ))
    ;; (display-buffer (get-buffer-create buffer-name))
	;; (when (functionp after-func)
	;; (funcall after-func))
    ;; )
    )
  )

(defun jong-project-exec-after-func (directory cmd)
  (interactive)
  ;; After process get started
  (let ((buffer-name (format "*jong-command-%s-%s*" directory cmd))
        )
    (with-current-buffer (get-buffer buffer-name)
	  (compilation-mode)
	  (compilation-shell-minor-mode)
      )
    )
  )

(defun jong-project-compile-project ()
  (interactive)
  (jong-common-reload-dir-locals)
  (if (and (boundp 'jong-project-compile-default-dir) (boundp 'jong-project-compile-command))
	  (jong-project-exec-command jong-project-compile-default-dir jong-project-compile-command)
	(message "\"projectile-project-root\" and \"jong-project-compile-command were not binded."))
  )


;; TODO
(defun jong-project-compile-next-error ()
  (interactive)
  (with-current-buffer (get-buffer jong-project-compile-command)
	)
  )

(defun jong-project-run-project (&optional num)
  (interactive "p")
  ;; (print num)
  (jong-common-reload-dir-locals)
  (cond ((equal num 1)
		 (if (and (boundp 'jong-project-run-default-dir) (boundp 'jong-project-run-command))
			 (jong-project-exec-command
              jong-project-run-default-dir
              jong-project-run-command)
           (message "\"projectile-project-root\" and \"jong-project-run-command were not binded.")))

		((equal num 2)
		 (if (and (boundp 'jong-project-sub-default-dir-2) (boundp 'jong-project-sub-command-2))
			 (jong-project-exec-command
              jong-project-sub-default-dir-2
              jong-project-sub-command-2)
		   (message "\"jong-project-sub-default-dir-2\" and \"jong-project-sub-command-2\".")))

		((equal num 3)
		 (if (and (boundp 'jong-project-sub-default-dir-3) (boundp 'jong-project-sub-command-3))
			 (jong-project-exec-command
              jong-project-sub-default-dir-3
              jong-project-sub-command-3)
		   (message "\"jong-project-sub-default-dir-3\" and \"jong-project-sub-command-3\".")))
		))



(defun jong-project-debug-project (&optional project-name)
  (interactive "p")
  (let ((debug-default-dir jong-project-debug-default-dir)
		(debug-cmd jong-project-debug-command))
	(jong-common-reload-dir-locals)
	(if (and (> (length debug-default-dir) 0) (> (length debug-cmd) 0))
		(progn
		  (setq default-directory debug-default-dir)
		  (cond
		   ((equal major-mode 'c++-mode)
			(call-interactively 'gud-gdb))
		   ((equal major-mode 'go-mode) (dlv debug-cmd))))
	  (message "\"projectile-project-root\" and \"jong-project-debug-command were not binded."))
	)
  )

(defun jong-project-subcmd-list ()
  (interactive)
  (let ((subcmds-temp)
		(cmd)
		(cmd-cons))
	(when (not (boundp 'jong-project-subcmds))
	  nil)
	(when (not (listp 'jong-project-subcmds))
	  nil)
	
	(setq subcmds-temp (mapcar
						(lambda (cmd)
						  (cons (format "%s: \"%s\", %s" (nth 0 cmd) (nth 1 cmd) (nth 2 cmd)) cmd))
						jong-project-subcmds))
	
	(helm :sources (helm-build-sync-source "Jong project Commands"
					 :candidates subcmds-temp
					 :fuzzy-match t
					 :action (lambda (cmd)
							   ;; (jong-project-subcmd-exec (nth 0 cmd) (nth 1 cmd) (nth 2 cmd))
                               (jong-project-exec-command
                                (nth 2 cmd)
                                (nth 1 cmd)
                                ;; (jong-project-exec-after-func (nth 2 cmd) (nth 1 cmd))
                                )
                               ))
		  :buffer "*jong project commands*")
	)
  )

(defun jong-project-subcmd-exec (title cmd directory)
  (let ((default-directory (if (not (file-directory-p directory))
							   (error "The directory was not existing")
							 directory))
		(proc)
		(output-buffer (format "*jong-project-subcmd <%s-%s>*" title directory))
		(target-window)
		(base-window (selected-window)))
	(unless (file-directory-p directory)
	  (error (format "Couldnt find the directory %s\"" directory)))

	(when (or (equal cmd nil) (string= cmd ""))
	  (error (format "Couldnt find  %s\"" cmd)))
    
	(when (get-buffer output-buffer) (kill-buffer output-buffer))

	(with-current-buffer (get-buffer-create output-buffer)
	  ;; (compilation-modet)
	  (ansi-color-for-comint-mode-on)
	  (comint-mode)
	  (setq default-directory directory)
	  (display-buffer (current-buffer))
	  (setq proc (start-process-shell-command
                  output-buffer
                  (current-buffer)
                  cmd))
	  (set-process-filter proc 'comint-output-filter)
      )
    )
  )


(setq jong-project-window-buffer-pair (make-hash-table :test 'equal))


(defun jong-project-register-buffer-to-show()
  (interactive)

  (let ((buffer-name)
		(candidates))
	
	(message-box "selected window:%s" (selected-window))
	(setq candidates (mapcar
					  (lambda (buffer)
						(buffer-name buffer))
					  (buffer-list)))
	(helm :sources (helm-build-sync-source "Jong project Commands2"
					 :candidates candidates
					 :fuzzy-match t
					 :action (lambda (buffer-name)
							   (message "buffer:%s" buffer-name)
							   (puthash (selected-window) buffer-name jong-project-window-buffer-pair)
							   )
					 )
		  :buffer "*jong project commands2*")
	)
  )


(defun jong-project-show-buffer-fixed-window()
  (interactive)
  (let ((target-window))
	(maphash (lambda (key val)
			   (if (window-live-p key)
				   (set-window-buffer key val)
				 (remhash key jong-project-window-buffer-pair))
			   )
			 jong-project-window-buffer-pair)
	)
  )

(global-set-key (kbd "C-c c m") 'jong-project-make-dot-dir-locals-el)
(global-set-key (kbd "C-c c v") 'jong-project-visit-dot-dir-locals-el)
(global-set-key (kbd "C-c c c") 'jong-project-compile-project)
(global-set-key (kbd "C-c c r") 'jong-project-run-project)
(global-set-key (kbd "C-c c d") 'jong-project-debug-project)
(global-set-key (kbd "C-c c l") 'jong-project-subcmd-list)
(global-set-key (kbd "C-c c .") 'jong-project-show-buffer-fixed-window)
(global-set-key (kbd "C-c c ,") 'jong-project-register-buffer-to-show)



(provide 'jong-project)
