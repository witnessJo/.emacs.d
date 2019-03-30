(require 'comint)

;;; Code:

(defconst jong-common-output-buffer "*jong-common-output*")
(defconst jong-common-debug-buffer "*jong-common-debug*")

(defvar-local jong-common-build-command nil)
(defvar-local jong-common-compile-default-dir nil)
(defvar-local jong-common-compile-command nil)
(defvar-local jong-common-run-default-dir nil)
(defvar-local jong-common-run-command nil)
(defvar-local jong-common-debug-default-dir nil)
(defvar-local jong-common-debug-command nil)

(put 'jong-common-build-command 'safe-local-variable #'stringp)
(put 'jong-common-compile-default-dir 'safe-local-variable #'stringp)
(put 'jong-common-compile-command 'safe-local-variable #'stringp)
(put 'jong-common-run-default-dir 'safe-local-variable #'stringp)
(put 'jong-common-run-command 'safe-local-variable #'stringp)
(put 'jong-common-debug-default-dir 'safe-local-variable #'stringp)
(put 'jong-common-debug-command 'safe-local-variable #'stringp)

(setq enable-local-variables t)
(setq enable-local-eval t)


(defun jong-common-send-command-to-buffer (cmd &optional buffer)
  "Send string to the sub process"
  (let ((target-buffer nil))
	 (if (equal buffer nil)
		  (setq target-buffer buffer)
	   (setq target-buffer (current-buffer)))
	 (with-current-buffer target-buffer
	   (comint-send-input cmd))
	 )
  )


(defun jong-common-walkup-and-find-file (FILENAME &optional DIRECTORY)
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
        (jong-common-walkup-and-find-file FILENAME parent-dir))
      )
    )
  )


(defun jong-common-walkup-and-find-dot-dir-locals-el()
  (interactive)
  (let ((file-name ".dir-locals.el")
		  (file-path))
	 (setq file-path (jong-common-walkup-and-find-file file-name))
	 (if file-path
		  (find-file file-path)
	   (message "Couldnt find the \".dir-locals.el\""))
	 )
  )


(defun jong-common-walkup-and-find-projectile-project-root ()
  (interactive)
  "Find .projectile and return project-root-path for projectile."
  (let ((dot-projectile ".projectile")
        (project-path))
    (setq project-path (jong-common-walkup-and-find-file dot-projectile))
    (if (equal project-path nil)
        (progn
          (message "Couldnt find a dot-projectile file.")
          "")
      (file-name-directory (directory-file-name project-path)))
    )
  )


(defun jong-common-make-dot-dir-locals-el ()
  (interactive)
  (let ((target-directory)
        (target-path)
        (template))
    (setq target-directory (jong-common-walkup-and-find-projectile-project-root))
    (unless (file-exists-p target-directory)
      (error "Couldnt find \".projectile\" file"))
    
    (setq target-path (format "%s%s" target-directory ".dir-locals.el"))
    (unless (file-exists-p target-path)
	   (setq template
            (concat "(\n"
                    (concat
                     "(nil . (\n")
                    (format "(projectile-project-root . \"%s\")\n" target-directory)
                    (format "(jong-common-build-command . \"none\")\n")
					     (format "(jong-common-compile-default-dir . \"%s\")\n" target-directory)
                    (format "(jong-common-compile-command . \"none\")\n")
					     (format "(jong-common-run-default-dir . \"%s\")\n" target-directory)
                    (format "(jong-common-run-command . \"none\")\n")
					     (format "(jong-common-debug-default-dir . \"%s\")\n" target-directory)
                    (format "(jong-common-debug-command . \"none\")\n")
                    "))\n"
					     ")\n"
					     ))
	   (write-region template nil target-path))
	 ;; (print template)
	 (find-file target-path))
  )


(defun jong-common-visit-dot-dir-locals-el ()
  (interactive)
  (let ((target-directory)
        (target-path))
    
    (setq target-directory (jong-common-walkup-and-find-projectile-project-root))
    (unless (file-exists-p target-directory)
	   (error "Couldnt find \".projectile\" file"))
    (setq target-path (format "%s%s" target-directory ".dir-locals.el"))
    (find-file target-path)
    )
  )

(defun jong-common-build-project ()
  (interactive)
  (let ((cmd))
    (with-current-buffer (get-buffer-create jong-common-output-buffer)
      (if (and (boundp 'projectile-project-root) (boundp 'jong-common-build-command))
          (progn
            (jong-reload-dir-locals-for-current-buffer)
            (setq cmd (format "%s/%s" projectile-project-root jong-common-build-command))
            (insert (format "Run the command $ %s" cmd))
            (display-buffer (current-buffer))
            (setq default-directory projectile-project-root)
            (ignore-errors (async-shell-command jong-common-build-command (current-buffer) (current-buffer)))
            )
        (message "\"projectile-project-root\" and \"jong-common-build-command were not binded."))
      )
    )
  )

(defun jong-common-compile-project ()
  (interactive)
  (let ((cmd))
    (with-current-buffer (get-buffer-create jong-common-output-buffer)
      (if (and (boundp 'jong-common-compile-default-dir) (boundp 'jong-common-compile-command))
          (progn
            (jong-reload-dir-locals-for-current-buffer)
            (setq default-directory jong-common-compile-default-dir)
            (setq cmd (format "%s/%s" jong-common-compile-default-dir jong-common-compile-command))
            (insert (format "Run the command $ %s" cmd))
            (display-buffer (current-buffer))
            (ignore-errors (async-shell-command jong-common-build-command (current-buffer) (current-buffer)))
            )
        (message "\"projectile-project-root\" and \"jong-common-build-command were not binded."))
      )
    )
  )

(defun jong-common-run-project ()
  (interactive))

(defun jong-common-debug-project ()
  (interactive))

(defun jong-test ()
  (interactive)
  )

(global-set-key (kbd "C-c c m") 'jong-common-make-dot-dir-locals-el)
(global-set-key (kbd "C-c c v") 'jong-common-visit-dot-dir-locals-el)
(global-set-key (kbd "C-c c b") 'jong-common-build-project)
(global-set-key (kbd "C-c c c") 'jong-common-compile-project)
(global-set-key (kbd "C-c c d") 'jong-common-debug-project)


(provide 'jong-common)

