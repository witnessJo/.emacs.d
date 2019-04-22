;;; Code:

(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
	(define-key key-translation-map [C-kanji] (kbd "C-SPC"))
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; Alias coding system for windows ;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(define-coding-system-alias 'cp65001 'utf-8)
	(load-theme 'sanityinc-tomorrow-blue t)
	(message "Windows")
	))
 ((string-equal system-type "darwin") ; Mac OS X
  (progn
	(message "Mac OS X")))
 ((string-equal system-type "gnu/linux") ; linux
  (progn
	(load-theme 'solarized-dark t)
	(message "Linux"))))



(provide 'jong-env-setting)
