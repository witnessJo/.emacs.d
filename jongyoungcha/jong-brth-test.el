;;; Code


(defcustom jong-brth-node-list
  (list "192.168.55.100"
		"192.168.55.101"
		"192.168.55.102"
		"192.168.55.103"))



(setq jong-brth-test-datadir (format "%s/%s" (getenv "HOME") "testdir"))
(setq jong-brth-highlights
	  '(("test\\|success\\|done\\|" . font-lock-function-name-face)
		("failed\\|" . font-lock-warning-face)))


(define-derived-mode jong-brth-attach-mode eshell-mode "BRTH-ATTCH"
  (setq font-lock-defaults '(jong-brth-highlights)))


(defun jong-brth-test-exec-cmd (cmd)
  (eshell-return-to-prompt)
  (insert cmd)
  (eshell-send-input))

(defun jong-brth-test-log ()
  (interactive)
  (let ((buffer-name "*brth-log*"))
	(with-current-buffer (get-buffer-create buffer-name)
	  (jong-brth-attach-mode)
	  (display-buffer (current-buffer))
	  (jong-brth-test-exec-cmd "cd")
	  (jong-brth-test-exec-cmd "tail -f geth.log")
	  )
	)
  )

(defun jong-brth-test-attach ()
  (interactive)
  (let ((buffer-name "*brth-attach*"))
	(with-current-buffer (get-buffer-create buffer-name)
	  (jong-brth-attach-mode)
	  (display-buffer (current-buffer))
	  (jong-brth-test-exec-cmd "geth attach"))
	)
  )

(defun jong-brth-test-new-account ()
  (interactive)
  (let ((buffer-name "*brth-attach*"))
	(with-current-buffer (get-buffer buffer-name)
	  (display-buffer (current-buffer))
	  (jong-brth-test-exec-cmd "personal.newAccount(\"jongyoungcha\")")
	  )
	)
  )


(define-derived-mode jong-brth-log-mode eshell-mode "BRTH-LOG"
  (setq font-lock-defaults '(jong-brth-highlights)))


(defun jong-brth-test-log ()
  (interactive)
  )


(provide 'jong-brth-test)
