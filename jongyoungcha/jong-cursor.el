;;; Code


(set (make-local-variable 'jong-beginning-of-line-text-pos) (point))
(defun jong-beginning-of-line-text()
  "Beginning of line."
  (interactive)
  (let ()
	(if (equal jong-beginning-of-line-text-pos (point))
		(beginning-of-line)
	  (beginning-of-line-text))
	(setq jong-beginning-of-line-text-pos (point))
	)
  )



(provide 'jong-cursor)
