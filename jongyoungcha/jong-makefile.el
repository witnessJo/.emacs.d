
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



(add-hook 'makefile-mode-hook
		  (lambda ()
			(setq-local indent-line-function 'jong-makefile-indent-line)
			(setq indent-tabs-mode t))
		  )

(provide 'jong-makefile)
