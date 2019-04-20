;;; Code

(require 'cl)

(use-package dash
  :ensure t
  :config)

(defgroup jong-common nil
  "Setting for `jong-common."
  :link nil)


(defun jong-common-set-window-1-2 (&optional left-buffer right-top-buffer right-bot-buffer)
  (interactive)
  (let ((wind-base)
		(wind-left)
		(wind-right-top)
		(wind-right-bot))
	(delete-other-windows)
	
	;; Make windows
	(setq wind-base (selected-window))
	(split-window-right)
	(setq wind-left (selected-window))
	(other-window 1)
	(setq wind-right-top (selected-window))
	(split-window-below)
	(other-window 1)
	(setq wind-right-bot (selected-window))
	
	;; Alloc buffers to windows
	(ignore-errors
	  (when (and (stringp left-buffer) (get-buffer left-buffer))
		(select-window wind-left)
		(switch-to-buffer (get-buffer left-buffer))))
	
	(ignore-errors
	  (when (and (stringp right-top-buffer) (get-buffer right-top-buffer))
		(select-window wind-right-top)
		(switch-to-buffer (get-buffer right-top-buffer))))
	
	(ignore-errors
	  (when (and (stringp right-bot-buffer) (get-buffer right-bot-buffer))
		(select-window wind-right-bot)
		(switch-to-buffer (get-buffer right-bot-buffer))))
	
	(select-window wind-base)
	))


(defun jong-common-set-window-2-1 (&optional left-top-buffer left-bot-buffer right-buffer)
  (interactive)
  (let ((wind-base)
		(wind-left-top)
		(wind-left-bot)
		(wind-right))
	(delete-other-windows)
	
	;; Make windows
	(setq wind-base (selected-window))
	
	(split-window-right)
	(split-window-below)
	(setq wind-left-top (selected-window))
	(other-window 1)
	(setq wind-left-bot (selected-window))
	(other-window 1)
	(setq wind-right (selected-window))
	
	;; Alloc buffers to windows
	(ignore-errors
	  (when (and (stringp left-top-buffer) (get-buffer left-top-buffer))
		(select-window wind-left-top)
		(switch-to-buffer (get-buffer left-top-buffer))))
	
	(ignore-errors
	  (when (and (stringp left-bot-buffer) (get-buffer left-bot-buffer))
		(select-window wind-left-bot)
		(switch-to-buffer (get-buffer left-bot-buffer))))
	
	(ignore-errors
	  (when (and (stringp right-buffer) (get-buffer right-buffer))
		(select-window wind-right)
		(switch-to-buffer (get-buffer right-buffer))))
	
	(select-window wind-base)
	))


(defun jong-common-set-window-4 (&optional buffer-left-top buffer-right-top buffer-left-bot buffer-right-bot)
  (interactive)
  (let ((wind-base)
		(wind-left-top)
		(wind-right-top)
		(wind-left-bot)
		(wind-right-bot))
	(delete-other-windows)

	;;Make Windows
	(setq wind-base (selected-window))
	
	(setq wind-left-top (selected-window))
	(split-window-below)
	(split-window-right)
	(other-window 1)
	(setq wind-right-top (selected-window))
	(other-window 1)
	(setq wind-left-bot (selected-window))
	(split-window-right)
	(other-window 1)
	(setq wind-right-bot (selected-window))

	;; Alloc buffers to windows
	(ignore-errors
	  (when (and (stringp buffer-left-top) (get-buffer buffer-left-top))
		(select-window wind-left-top)
		(switch-to-buffer (get-buffer buffer-left-top))))

	(ignore-errors
	  (when (and (stringp buffer-right-top) (get-buffer buffer-right-top))
		(select-window wind-right-top)
		(switch-to-buffer (get-buffer buffer-right-top))))

	(ignore-errors
	  (when (and (stringp buffer-left-bot) (get-buffer buffer-left-bot))
		(select-window wind-left-bot)
		(switch-to-buffer (get-buffer buffer-left-bot))))

	(ignore-errors
	  (when (and (stringp buffer-right-bot) (get-buffer buffer-right-bot))
		(select-window wind-right-bot)
		(switch-to-buffer (get-buffer buffer-right-bot))))
	
	(select-window wind-base)
	)
  )

;; (defun jong-common-test ()
;; (interactive)
;; (jong-common-set-window-4 "jong-common.el" "*Messages*" "*GNU Emacs*" "*scratch*")
;; )

(defun jong-common-find-buffer-and-move (target-buffer-name)
  "Move to the window having the TARGET-BUFFER-NAME of the buffer."
  (let ((buffer-name))
	(if (stringp target-buffer-name)
		(dolist (target-window (window-list))
		  (when (equal target-buffer-name (buffer-name (window-buffer target-window)))
			(select-window target-window)))
	  )
	)
  )



(defun jong-common-find-file-other-window ()
  (interactive)
  (let ((buffer-source (current-buffer))
		
		(buffer-target))
	(call-interactively 'helm-find-files)
	(setq buffer-target (current-buffer))
	(unless (eq buffer-source buffer-target)
	  (progn
		(pop-to-buffer buffer-target 'other-window)
		(other-window -1)
		(pop-to-buffer-same-window buffer-source)
		))
	)
  )


(defun jong-common-process-exit-code-and-output (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (interactive)
  (with-temp-buffer
	(list (apply 'call-process program nil (current-buffer) nil args)
		  (buffer-string)))
  )

(defun jong-common-find-file-other-window-string (string)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (let ((file-path)
		(target-line)
		(regex-pattern "^\\(.+\\):\\(.+\\)$"))
	(if (and (stringp string) (string-match regex-pattern string))
		(progn
		  (setq file-path (match-string 1 string))
		  (setq target-line (string-to-number (match-string 2 string)))
		  (find-file-other-window file-path)
		  (with-no-warnings (goto-line target-line))
		  (other-window -1))
	  (message "regex buildind failed...")
	  )
	)
  )


(global-set-key (kbd "C-c C-f") 'jong-common-find-file-other-window)



(provide 'jong-common)

