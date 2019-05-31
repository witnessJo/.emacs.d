;;; Code:

(defcustom jong-env-locale-value "utf-8"
  "A variable for locale language setting.")

(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
	  
	  (define-key key-translation-map [C-kanji] (kbd "C-SPC"))
	  
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  ;; Alias coding system for windows ;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	  (set-language-environment "Korean")
	  (define-coding-system-alias 'cp65001 'utf-8)
	  (setq jong-env-locale-value 'utf-8)
	  (set-default-coding-systems jong-env-locale-value)
	  (set-input-method 'korean-hangul)
	  
	  (load-theme 'sanityinc-tomorrow-blue t)
	  ))
 
 ((string-equal system-type "darwin") ; Mac OS X
  (progn
	  (message "Mac OS X")))
 
 ((string-equal system-type "gnu/linux") ; linux
  (progn
	  (set-language-environment "Korean")
	  (load-theme 'solarized-dark t)
	  (with-no-warnings (setq projectile-indexing-method 'hybrid))
    ;; (load-theme 'sanityinc-tomorrow-night t)
	  ;; (load-theme 'sanityinc-tomorrow-blue t)
	  (setq jong-env-locale-value
		      (if (string= (getenv "LANG") "ko_KR.utf8") 'utf-8 'euc-kr))

	  (message "Linux"))))

(when (member "Consolas" (font-family-list))
  (set-face-attribute 'default nil :font "Consolas-12")
  (setq-default line-spacing 2))

(prefer-coding-system jong-env-locale-value)
(set-default-coding-systems jong-env-locale-value)
(set-language-environment 'UTF-8)
(set-input-method 'korean-hangul)
(setq-default file-name-coding-system jong-env-locale-value)
(setq-default locale-coding-system jong-env-locale-value)
(set-terminal-coding-system jong-env-locale-value)
(set-keyboard-coding-system jong-env-locale-value)
(set-selection-coding-system jong-env-locale-value)
(dynamic-completion-mode)

(setq-default indent-tabs-mode t)
(setq-default tab-width 4)

(set-cursor-color "#aa4444")
(set-face-background #'hl-line "#004500")
(global-hl-line-mode t)


;; For protecting my eyes...
(custom-set-faces
 '(flymake-errline ((((class color)) (:background "#444444"))))
 '(flymake-warnline ((((class color)) (:background "#4444aa")))))


(provide 'jong-env-setting)
