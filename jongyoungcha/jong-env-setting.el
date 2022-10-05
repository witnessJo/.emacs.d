;;; Code:

(define-coding-system-alias 'UTF-8 'utf-8)
(defcustom jong-env-locale-value "utf-8"
  "A variable for locale language setting.")

(cond
 ((string-equal system-type "windows-nt") ; Micro©gsoft Windows
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
	(message "Windows")
    ))

 ((string-equal system-type "darwin")
                                        ; Mac OS X
  (setq default-frame-alist '((font . "Source Code Pro-15")
							  (vertical-scroll-bars . 0)
							  ))
  (load-theme 'solarized-dark-high-contrast t)
  (progn
    (setq shell-file-name "zsh")
    (setq shell-command-switch "-ic")
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'alt)
	(set-input-method 'korean-hangul)
	(message "Mac OS X")
	))
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    (setq default-frame-alist '((font . "Source Code Pro-12")
							    (vertical-scroll-bars . 0)
							    ))
    ;; (load-theme 'deeper-blue t)
    
    (set-language-environment "Korean")
    (with-no-warnings (setq projectile-indexing-method 'hybrid))
    
    (setq jong-env-locale-value
		  (if (string= (getenv "LANG") "ko_KR.utf8") 'utf-8 'euc-kr))
    (prefer-coding-system jong-env-locale-value)
    (set-default-coding-systems jong-env-locale-value)
    (set-language-environment 'utf-8)
    (set-input-method 'korean-hangul)
    (setq-default file-name-coding-system jong-env-locale-value)
    (setq-default locale-coding-system jong-env-locale-value)
    (set-terminal-coding-system jong-env-locale-value)
    (set-keyboard-coding-system jong-env-locale-value)
    (set-selection-coding-system jong-env-locale-value)
    (dynamic-completion-mode)
    (message "Linux")
	))
 )

(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(set-cursor-color "#aa4444")
(set-face-background #'hl-line "#004500")
(global-hl-line-mode t)
(show-paren-mode t)
(delete-selection-mode t)
(load-theme 'solarized-zenburn t)
;; (load-theme 'solarized-dark-high-contrast t)
;; (load-theme 'sanityinc-tomorrow-blue t)

(global-eldoc-mode -1)
(auto-save-visited-mode t)
(global-auto-revert-mode t)
;; (global-visual-line-mode t)


;; For protecting my eyes...
(custom-set-faces
 '(flymake-errline ((((class color)) (:background "#444444"))))
 '(flymake-warnline ((((class color)) (:background "#4444aa"))))
 )

;; aliases
(defalias 'yes-or-no-p 'y-or-n-p)


(provide 'jong-env-setting)
