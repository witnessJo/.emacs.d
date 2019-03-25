
(require 'comint)

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


(provide 'jong-common)
