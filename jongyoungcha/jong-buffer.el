;;; Code:

(defun buffer-backed-by-file-p (buffer)
  (let ((backing-file (buffer-file-name buffer)))
    (if (buffer-modified-p buffer)
        t
      (if backing-file
          (file-exists-p (buffer-file-name buffer))
        t))))


(run-with-timer 60 60 (lambda ()
						(mapc 'kill-buffer
							  (-remove 'buffer-backed-by-file-p (buffer-list)))))


(provide 'jong-buffer)

