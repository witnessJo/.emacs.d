;;; Code:

(defgroup jong-bash nil
  "Jongyoungcha bash."
  :prefix "jong-bash-"
  :group 'jong-bash)

(defconst jong-bash-output-buffer "*jong-bash-output*")


(defconst jong-bash-bashdb-buffer "*jong-bash-bashdb*")
(defcustom jong-bash-bashdb-exec-command "bashdb"
  "Command for bashdb.")


(define-derived-mode jong-bash-bashdb-mode gud-mode "jong-bash-bashdb"
  )


(use-package bash-completion
  :ensure t
  :config
  (autoload 'bash-completion-dynamic-complete
    "bash-completion"
    "BASH completion hook")
  (add-hook 'shell-dynamic-complete-functions
			'bash-completion-dynamic-complete))


(defun jong-bash-install-bashdb()
  (interactive)
  (let ((bash-bashdb-url "https://sourceforge.net/projects/bashdb/files/bashdb/4.4-1.0.1/bashdb-4.4-1.0.1.tar.gz")
		(file-name "bashdb-4.4-1.0.1.tar.gz")
		(target-directory "bashdb-4.4-1.0.1"))
	(with-current-buffer (get-buffer-create jong-bash-bashdb-buffer)
	  (display-buffer (current-buffer))
	  (async-shell-command
	   (concat "cd;"
			   (format "wget %s;" bash-bashdb-url)
			   (format "tar -xvf %s;" file-name)
			   (format "cd %s;" target-directory)
			   "./configure; make;"
			   "sudo make install")
	   (current-buffer)
	   (current-buffer)))
	)
  )


(defun jong-bash-bashdb ()
  (interactive)
  (let ((ret (car (jong-common-process-exit-code-and-output "which" "bashdb"))))
	(if (equal ret 0)
		(with-current-buffer (get-buffer-create jong-bash-bashdb-buffer)
		  (display-buffer (current-buffer))
		  (setq jong-bash-bashdb-exec-command (read-string "bashdb command : " jong-bash-bashdb-exec-command))
		  (ignore-errors (async-shell-command jong-bash-bashdb-exec-command
											  (current-buffer) (current-buffer)))
		  (jong-bash-bashdb-mode)
		  (message "Couldnt find \"bashdb\""))
	  (message "Couldnt find the bashdb binary..."))
	)
  )


(defun jong-bash-bashdb-send-command (buffer cmd)
  (interactive)
  (let ((process (get-buffer-process buffer)))
	(if process
		(with-current-buffer buffer
		  (goto-char (point-max))
		  (insert (format "%s" cmd))
		  (comint-send-input))
	  (message "Invalid buffer arguments (not buffer type of dont have a process).")
	  )
	)
  )


(defun jong-bash-remap-command (cmd)
  (interactive)
  (let ((syntax-list (split-string cmd))
		(syntax-head)
		(syntax-head-map '(("p" . "print")
						   ("f" . "frame")
						   ("b" . "break")
						   ("bt" . "backtrace")
						   ("c" . "continue")))
		(syntax-head-searched)
		(merged-command))
	(if (setq syntax-head-searched (cdr (assoc (car syntax-list) syntax-head-map)))
		(progn
		  (setq syntax-list (cons syntax-head-searched (cdr syntax-list)))
		  (setq merged-command (mapconcat 'identity syntax-list " "))
		  (message merged-command)
		  merged-command)
	  (message (format "Couldnt find the mappped syntax. (%s)" syntax-head)))
	)
  )


(defun jong-bash-bashdb-next ()
  (interactive)
  (with-current-buffer (get-buffer jong-bash-bashdb-buffer)
	(jong-bash-bashdb-send-command (current-buffer) "next"))
  )


(defun jong-bash-bashdb-step-into ()
  (interactive)
  (with-current-buffer (get-buffer jong-bash-bashdb-buffer)
	(jong-bash-bashdb-send-command (current-buffer) "step+")))


(defun jong-bash-bashdb-step-out ()
  (interactive)
  (with-current-buffer (get-buffer jong-bash-bashdb-buffer)
	(jong-bash-bashdb-send-command (current-buffer) "step-")))


(defun jong-bash-bashdb-continue ()
  (interactive)
  (with-current-buffer (get-buffer jong-bash-bashdb-buffer)
	(jong-bash-bashdb-send-command (current-buffer) "continue")))


(defun jong-bash-bashdb-break ()
  (interactive)
  (let ((break-command)
        (bashdb-buffer))
    (with-current-buffer (current-buffer)
	  (cond ((equal mode-name "Shell-script")
             (progn
			   (setq break-command (format "break %s:%s" (buffer-file-name) (line-number-at-pos)))
			   (if (setq bashdb-buffer (get-buffer jong-bash-bashdb-buffer))
				   (with-current-buffer (get-buffer jong-bash-bashdb-buffer)
                     (display-buffer (current-buffer))
                     (jong-bash-bashdb-send-command (current-buffer) break-command))
                 (message "Couldnt find the bashdb-buffer."))))
            
		    ((equal mode-name "jong-bash-bashdb")
             (jong-bash-bashdb-send-command (current-buffer) "break"))
            
		    (t "Invalid buffer to exec jong-bash-bashdb-break()")))
    )
  )


(defun jong-bash-set-bash-key ()
  (interactive)

  )


(defun jong-test()
  (interactive)
  (jong-bash-remap-command "p asdf sdafsda fsdaf")
  )

(provide 'jong-bash)
