;;; Code

(require 'cl)

(use-package dash
  :ensure t
  :config)

(defgroup jong-common nil
  "Setting for `jong-common."
  :link nil)


(defvar jong-common-navi-ring ())
(defvar jong-common-navi-queue-max 50)
(defvar jong-common-navi-curr-pos 0)

(defun jong-common-navi-push-jong-ring (mark)
  "Dock."
  (let ((length-of-ring (if (equal jong-common-navi-ring nil) 0 (length jong-common-navi-ring)))
		)
	(when (< length-of-ring jong-common-navi-queue-max)
	  (if (equal jong-common-navi-ring nil)
		  (setq jong-common-navi-ring (list mark))
		(progn
		  (message "%d !!!!" length-of-ring)
		  (add-to-list 'jong-common-navi-ring mark)
		  ))
	  )
	)
  )

(defun jong-common-navi-show-buffer-with-pos (string)
  )
 


(defun jong-common-navi-move-pos-forward ()
  (interactive)
  )

(defun jong-common-navi-move-pos-backward ()
  (interactive)
  )

(add-hook 'deactivate-mark-hook (lambda ()
								  (jong-common-navi-push-jong-ring (car mark-ring))))



(defun jong-common-find-file-other-window()
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
        )))
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

