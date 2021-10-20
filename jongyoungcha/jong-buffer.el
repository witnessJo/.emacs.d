;;; Code:

(defun buffer-backed-by-file-p (buffer)
  (let ((backing-file (buffer-file-name buffer)))
    (if (buffer-modified-p buffer)
        t
      (if backing-file
          (file-exists-p (buffer-file-name buffer))
        t))))


(defun jong-buffer-throw-left()
  (interactive)
  (buf-move-left)
  (windmove-right)
  )

(defun jong-buffer-throw-right()
  (interactive)
  (buf-move-right)
  (windmove-left)
  )

(defun jong-buffer-throw-up()
  (interactive)
  (buf-move-up)
  (windmove-down)
  )

(defun jong-buffer-throw-down()
  (interactive)
  (buf-move-down)
  (windmove-up)
  )


(run-with-timer 60 60 (lambda ()
						(mapc 'kill-buffer
							  (-remove 'buffer-backed-by-file-p (buffer-list)))))


(provide 'jong-buffer)

