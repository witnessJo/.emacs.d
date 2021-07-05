;; (use-package dotenv
;; :ensure t
;; :config
;; (dotenv :repo "pkulev/dotenv.el"
;; :fetcher github :upgrade t))


(require 'dotenv-mode)
(add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode))

(defun jong-dotenv-load-dotenv-test ()
  (interactive)
  (let (file-path)
	(setq file-path (format "%s/.env" (getenv "HOME")))
	(jong-dotenv-load-dotenv file-path)
	)
  )


(defun jong-dotenv-load-dotenv (file-path)
  (let (lines)
	(with-current-buffer (find-file-noselect file-path)
	  (setq lines (split-string
				   (save-restriction
					 (widen)
					 (buffer-substring-no-properties
					  (point-min)
					  (point-max)))
				   "\n" t))
	  )
	(print lines)
	)
  )


(provide 'dotenv-mode)
