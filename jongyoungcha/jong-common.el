(require 'comint)

;;; Code:

(defconst jong-common-output-buffer "*jong-common-output*")
(defconst jong-common-debug-buffer "*jong-common-debug*")

(setq enable-local-variables ":safe")

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

(defvar-local jong-common-build-command nil)
(defvar-local jong-common-compile-command nil)
(defvar-local jong-common-run-command nil)
(defvar-local jong-common-debug-command nil)

(defun jong-common-make-dot-dir-locals-el()
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
                    (format "(jong-common-build-command . nil)\n")
                    (format "(jong-common-compile-command . nil)\n")
                    (format "(jong-common-run-command . nil)\n")
                    (format "(jong-common-debug-command . nil)\n")
                    (format "))\n")
                    ")\n"))
          (print template)
          (write-region template nil target-path))
    ;;Set nil
    (find-file target-path)
    )
  )

(defun jong-common-build-project ()
  (interactive))

(defun jong-common-compile-project ()
  (interactive))

(defun jong-common-run-project ()
  (interactive))

(defun jong-common-debug-project ()
  (interactive))

(defun jong-test ()
  (interactive)
  )

;; (global-set-key)


(provide 'jong-common)
