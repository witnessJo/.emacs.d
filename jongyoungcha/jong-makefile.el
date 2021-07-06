
;;; Code:

(defun jong-makefile-indent-line ()
  (save-excursion
	(forward-line 0)
	(cond
	 ;; keep TABs
	 ((looking-at "\t")
	  t)
	 ;; indent continuation lines to 4
	 ((and (not (bobp))
		   (= (char-before (1- (point))) ?\\))
	  (delete-horizontal-space)
	  (indent-to 4))
	 ;; delete all other leading whitespace
	 ((looking-at "\\s-+")
	  (replace-match "")))))



(add-hook 'makefile-bsdmake-mode-hook
		  (lambda ()
			(message-box "makefile-bsdmake")
			(setq-local indent-line-function 'jong-makefile-indent-line)
			;; (setq-local indent-line-function 'c-indent-line)
			(setq indent-tabs-mode t)
			;; Bind the TAB key
			;; (global-set-key (kbd "TAB") 'self-insert-command)
			
			;; Set the tab width
			;; (setq default-tab-width 4)
			;; (setq tab-width 4)
			;; (setq c-basic-indent 4))
			)
		  )

(provide 'jong-makefile)
;;; jong-makefile.el ends here
