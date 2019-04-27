(require 'comint)

;;; Code:

(defconst jong-project-output-buffer "*jong-project-output*")
(defconst jong-project-debug-buffer "*jong-project-debug*")

(add-to-list 'jong-kill-buffer-patterns jong-project-output-buffer)
(add-to-list 'jong-kill-buffer-patterns jong-project-debug-buffer)

(defvar-local jong-project-build-command nil)
(defvar-local jong-project-compile-default-dir nil)
(defvar-local jong-project-compile-command nil)
(defvar-local jong-project-run-default-dir nil)
(defvar-local jong-project-run-command nil)
(defvar-local jong-project-debug-default-dir nil)
(defvar-local jong-project-debug-command nil)

(put 'jong-project-build-command 'safe-local-variable #'stringp)
(put 'jong-project-compile-default-dir 'safe-local-variable #'stringp)
(put 'jong-project-compile-command 'safe-local-variable #'stringp)
(put 'jong-project-run-default-dir 'safe-local-variable #'stringp)
(put 'jong-project-run-command 'safe-local-variable #'stringp)
(put 'jong-project-debug-default-dir 'safe-local-variable #'stringp)
(put 'jong-project-debug-command 'safe-local-variable #'stringp)

(setq enable-local-variables t)
(setq enable-local-eval t)


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
                    (format "(projectile-project-root . \"%s\")\n" target-directory)
                    (format "(jong-project-build-command . \"none\")\n")
					     (format "(jong-project-compile-default-dir . \"%s\")\n" target-directory)
                    (format "(jong-project-compile-command . \"none\")\n")
					     (format "(jong-project-run-default-dir . \"%s\")\n" target-directory)
                    (format "(jong-project-run-command . \"none\")\n")
					     (format "(jong-project-debug-default-dir . \"%s\")\n" target-directory)
                    (format "(jong-project-debug-command . \"none\")\n")
                    "))\n"
					     ")\n"
					     ))
	   (write-region template nil target-path))
	 (find-file target-path))
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
  
  (unless (file-directory-p directory)
	 (error (format "Couldnt find the directory %s\"" directory)))
  
  (when (or (equal cmd nil) (string= cmd ""))
	 (error (format "Couldnt find  %s\"" cmd)))
  
  (with-current-buffer (get-buffer-create jong-project-output-buffer)
	 (display-buffer (current-buffer))
	 (setq default-directory directory)
	 (insert (format "Run the command $ %s" cmd))
	 (goto-char (point-max))
	 (ignore-errors (async-shell-command cmd (current-buffer) (current-buffer)))
    (fundamental-mode)
    (when (functionp after-func)
      (funcall after-func))
    )
  )


(defun jong-project-build-project ()
  (interactive)
  (jong-reload-dir-locals-for-current-buffer)
  (if (and (boundp 'projectile-project-root) (boundp 'jong-project-build-command))
	   (jong-project-exec-command projectile-project-root jong-project-build-command)
	 (message "\"projectile-project-root\" and \"jong-project-build-command were not binded."))
  )


(defun jong-project-compile-project ()
  (interactive)
  (jong-reload-dir-locals-for-current-buffer)
  (if (and (boundp 'jong-project-compile-default-dir) (boundp 'jong-project-compile-command))
	   (jong-project-exec-command jong-project-compile-default-dir
                                 jong-project-compile-command
                                 (lambda ()
                                   (compilation-mode)))
    (message "\"projectile-project-root\" and \"jong-project-build-command were not binded."))
  )


(defun jong-project-run-project ()
  (interactive)
  (jong-reload-dir-locals-for-current-buffer)
  (if (and (boundp 'jong-project-run-default-dir) (boundp 'jong-project-run-command))
	   (jong-project-exec-command jong-project-run-default-dir jong-project-run-command)
	 (message "\"projectile-project-root\" and \"jong-project-build-command were not binded."))
  )


(defun jong-project-debug-project ()
  (interactive)
  (jong-reload-dir-locals-for-current-buffer)
  (if (and (boundp 'jong-project-debug-default-dir) (boundp 'jong-project-debug-command))
	   (jong-project-exec-command jong-project-debug-default-dir jong-project-debug-command)
	 (message "\"projectile-project-root\" and \"jong-project-build-command were not binded."))
  )


(global-set-key (kbd "C-c c m") 'jong-project-make-dot-dir-locals-el)
(global-set-key (kbd "C-c c v") 'jong-project-visit-dot-dir-locals-el)
(global-set-key (kbd "C-c c b") 'jong-project-build-project)
(global-set-key (kbd "C-c c c") 'jong-project-compile-project)
(global-set-key (kbd "C-c c r") 'jong-project-run-project)
(global-set-key (kbd "C-c c d") 'jong-project-debug-project)


(provide 'jong-project)

