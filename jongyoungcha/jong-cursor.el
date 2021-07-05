;;; Code:


(defvar-local jong-edit-beginning-of-line-text-pos (point))


(defun jong-edit-beginning-of-line-text()
  "Beginning of line."
  (interactive)
  (let ()
	(if (equal jong-edit-beginning-of-line-text-pos (point))
		(beginning-of-line)
	  (beginning-of-line-text))
	(setq jong-edit-beginning-of-line-text-pos (point))
	)
  )


(defun jong-edit-paste-text()
  (interactive)
  (let ()
	(if (use-region-p)
		(progn
		  (call-interactively 'delete-backward-char)
		  (call-interactively 'yank))
	  (call-interactively 'yank)))
  )

(defun jong-set-mark ()
  "jong-set-mark."
  (interactive)
  (setq this-command-keys-shift-translated t)
  (if (not (use-region-p))
	  (call-interactively 'set-mark-command)))

(defun jong-edit-change-hook(arg &optional killp)
  "jong-edit-change-hook."
  (interactive "P")
  (when (region-active-p)
	(call-interactively 'backward-delete-char-untabify)
	)
  )

(add-hook 'before-change-functions 'jong-edit-change-hook)


(provide 'jong-cursor)
