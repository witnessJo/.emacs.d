

(defun jong-shell-util-scp-upload ()
  (interactive)
  (let ()
	(with-current-buffer (get-buffer-create "jong-util-scp")
	  (setq target-file "main")
	  (shell-command (format "sshpass -p \"%s\" scp ./%s %s@10.19.45.181:/home/sentprc/"
							 (getenv "FSDC_PASSWD")
							 target-file
							 (getenv "FSDC_USER")))
	  ))
  )

(defun jong-shell-util-connect ()
  )


(provide 'jong-shell-util)
