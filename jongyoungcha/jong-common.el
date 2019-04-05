;;; Code


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





(global-set-key (kbd "C-c C-f") 'jong-common-find-file-other-window)
 


(provide 'jong-common)

