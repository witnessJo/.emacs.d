;;; Code:

(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
	;; (global-set-key [C-kanji] (lookup-key global-map (kbd "C-S")))
	;; (global-set-key [C-kanji] 'set-mark-command)
	(define-key key-translation-map [C-kanji] (kbd "C-SPC"))
	(message "Windows")
	))
 ((string-equal system-type "darwin") ; Mac OS X
  (progn
	(message "Mac OS X")))
 ((string-equal system-type "gnu/linux") ; linux
  (progn
	(message "Linux"))))



(provide 'jong-env-setting)
