;;; Code:

(use-package syntax-subword
  :ensure t)

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

(defun jong-cursor-delete-word-forward (arg)
  (interactive "p")
  (let ((prev-cursor-pos (point))
		(prev-line-num (line-number-at-pos))
		(next-line-num))
	(forward-word arg)
	(setq next-line-num (line-number-at-pos))
	(goto-char prev-cursor-pos)
	(if (equal prev-line-num next-line-num)
		(delete-region
		 (point)
		 (progn
		   (forward-word arg)
		   (point)))
	  (hungry-delete-forward arg))
	)
  )


(defun jong-cursor-forward-word (arg)
  (interactive "p")
  (forward-word arg)
  )

(defun jong-cursor-backward-word (arg)
  (interactive "p")
  (backward-word arg)
  )

(defun jong-cursor-delete-word-backward (arg)
  (interactive "p")
  (let ((prev-cursor-pos (point))
		(prev-line-num (line-number-at-pos))
		(next-line-num))
	(backward-word arg)
	(setq next-line-num (line-number-at-pos))
	(goto-char prev-cursor-pos)
	(if (equal prev-line-num next-line-num)
		(delete-region
		 (point)
		 (progn
		   (backward-word arg)
		   (point)))
	  (hungry-delete-backward arg))
	)
  )

(defun jong-cursor-kill-line (&optional arg)
  (interactive "P")
  (let ((begin-pos (progn (beginning-of-line) (point)))
		(end-pos (progn (end-of-line) (point))))
	(if (string-match-p "^[[:space:]]+$" (thing-at-point 'line t))
		(progn (beginning-of-line)
			   (print "")
			   (setq begin-pos (point)))
	  (progn (beginning-of-line)
			 (re-search-forward "[[:graph:]]")
			 (if (> (point) end-pos)
				 (goto-char begin-pos)
			   (backward-char 1))
			 (print "text")))
	(call-interactively 'kill-line)
	(beginning-of-line-text))
  )

(defun jong-cursor-delete-line ()
  (interactive)
  (print (thing-at-point 'line t))
  (if (string-match-p "^[ \t\n\r]+$" (thing-at-point 'line))
	  (progn
		(print "here!1")
		(delete-char 1))
	(progn
	  (print "here2")
	  (delete-region
	   (line-beginning-position)
	   (line-end-position))
	  )
	)
  (indent-according-to-mode)
  )

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

(defun jong-edit-paste-text-below()
  (interactive)
  (let ()
	(call-interactively 'jong-common-open-line-below)
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


(defun jong-cursor-move-eol-region()
  (interactive)
  (when (not (use-region-p))
	(command-execute 'set-mark-command))
  (end-of-line)
  )

(defun jong-cursor-move-bol-region()
  (interactive)
  (when (not (use-region-p))
	(command-execute 'set-mark-command))
  (jong-edit-beginning-of-line-text)
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

(defun jong-cursor-indent (offset)
  (let ((start-pos)
        (end-pos))
    (if (use-region-p)
        (progn (indent-code-rigidly
                (region-beginning)
                (region-end)
                offset)
               (setq deactivate-mark nil))
      (beginning-of-line)
      (setq start-pos (point))
      (end-of-line)
      (setq end-pos (point))
      (activate-mark)
      (indent-code-rigidly
       start-pos
       end-pos
       offset)
      (beginning-of-line-text)
      )
    )
  )

(defun jong-cursor-indent-left ()
  (interactive)
  (jong-cursor-indent -2))

(defun jong-cursor-indent-right ()
  (interactive)
  (jong-cursor-indent 2))

(provide 'jong-cursor)
