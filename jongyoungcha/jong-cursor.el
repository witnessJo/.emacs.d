;;; Code:

(defvar-local jong-edit-beginning-of-line-text-pos (point))


(defun jong-cursor-delete-subword-forward (arg)
  (interactive "p")
  (delete-region
   (point)
   (progn
	 (syntax-subword-forward arg)
	 (point)))
  )

(defun jong-cursor-delete-subword-backward (arg)
  (interactive "p")
  (delete-region
   (point)
   (progn
	 (syntax-subword-backward arg)
	 (point)))
  )


(defun jong-cursor-delete-word-forward (arg )
  (interactive "p")
  (delete-region
   (point)
   (progn
	 (forward-word arg)
	 (point))))


(defun jong-cursor-delete-word-backward (arg)
  (interactive "p")
  (delete-region
   (point)
   (progn
	 (backward-word arg)
	 (point))))

(defun jong-cursor-delete-line ()
  (interactive)
  (delete-region
   (progn
	 (beginning-of-line)
	 (point))
   (progn
	 (end-of-line)
	 (point))))


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

(defun jong-cursor-newline-align-above()
  (interactive)
  (when (region-active-p)
	(deactivate-mark))
  (beginning-of-line)
  (set-mark (point))
  (beginning-of-line-text)
  (call-interactively 'copy-region-as-kill)
  (end-of-line)
  (call-interactively 'electric-indent-just-newline)
  (insert (pop kill-ring))
  )

(defun jong-set-mark()
  "jong-set-mark."
  (interactive)
  (setq this-command-keys-shift-translated t)
  (if (not (use-region-p))
	  (call-interactively 'set-mark-command)))

(defun jong-edit-change-hook(arg &optional killp)
  "jong-edit-change-hook."
  (interactive "P")
  (when (use-region-p)
	(call-interactively 'delete-active-region))
  )

(defun jong-cursor-move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun jong-cursor-move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (jong-cursor-move-text-internal arg))

(defun jong-cursor-move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (jong-cursor-move-text-internal (- arg)))

(provide 'jong-cursor)
