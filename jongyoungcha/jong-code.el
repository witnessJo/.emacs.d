;;; Code:

(defun jong-code-comment ()
  (interactive)
  (let ((base-pos 0))
	(setq base-pos (point))
	(beginning-of-line)
	(call-interactively 'comment-line)
	(goto-char base-pos)
	(forward-line)
	(indent-for-tab-command)
	)
  )

(provide 'jong-code)
