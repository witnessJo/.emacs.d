;;; Code:
;;; message


(defcustom jong-sentbe-helm-repo nil
  "Just path value of sentbe helm repo."
  :type 'string
  :group 'jong-sentbe)


(defun jong-sentbe-copy-file-to-helm-demeter-pb()
  (interactive)
  (let ((src-path)
		(dst-path))
	(print "jong-sentbe-copy-file-to-helm-demeter-pb")
	(setq src-path (jong-sentbe-select-file))
	(setq dst-path (jong-sentbe-select-file))
	(print "src-path:")
	(print "dst-path:")
	)
  )

(defun jong-sentbe-select-file()
  (interactive)
  )

(provide 'jong-sentbe)
