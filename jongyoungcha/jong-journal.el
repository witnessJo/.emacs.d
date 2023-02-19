;;; Code

(defun jong-journal-open-journal ()
  (interactive)
  (find-file (format "%s/jongyoungcha/issues_free.org" (getenv "HOME")))
  )


(defun jong-journal-open-worklist ()
  (interactive)
  (find-file (format "%s/jongyoungcha/issues_work.org" (getenv "HOME")))
  )


(provide 'jong-journal)
