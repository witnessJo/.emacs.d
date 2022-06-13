;;; Code


(defun jong-bookmark-set-anonymous()
  "Add bookmark with buffer name with line position."
  (interactive)
  (let ((current-buffer-name (buffer-name (current-buffer)))
        (current-pos (line-number-at-pos)))
    (bookmark-set (format "%s:%d" current-buffer-name current-pos)))
  )


(provide 'jong-bookmark)
