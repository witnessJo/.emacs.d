;;; Code:

(require 'json)

(use-package json-mode
  :ensure t)

(use-package restclient
  :ensure t
  ;; :defer t
  :mode (("\\.http\\'" . restclient-mode))
  :bind (:map restclient-mode-map
			  ("C-c C-f" . json-mode-beautify)
			  ("C-c C-c" . (lambda()
							(interactive)
							(restclient-http-send-current nil t)))
			  )
  )

;; (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))
;; (eval-after-load "restclient"
;; (define-key restclient-mode-map (kbd "C-c C-c") 
;; )


(defcustom jong-http-restclient-response-json-elems nil
  "Name for response buffer."
  :group 'jong-http
  :type 'list)

(defcustom jong-http-temporary-value nil
  "Just temporary value for rest client mode."
  :group 'jong-http
  :type 'string)


(defun jong-http-restclient-parse-response-json()
  "Parse restclient ouput buffer."
  (let ((json-array-type 'list)
		(json-ret)
		(target-buffer (get-buffer restclient-same-buffer-response-name)))
	(if target-buffer
		(with-current-buffer target-buffer
		  (setq json-ret (json-read-from-string (buffer-string)))
		  json-ret)
	  (progn
		(message "Couldnt find %s" restclient-same-buffer-response-name)
		nil)
	  )
	)
  )


(defun jong-http-resclient-parse-response-json-and-getkey (key-to-find)
  (interactive)
  (let ((json-elems)
		(key)
		(value))
	(setq json-elems (jong-http-restclient-parse-response-json))
	(when json-elems
	  (catch 'found
		(dolist (elem json-elems)
		  (setq key (car elem))
		  (when (equal (symbol-name key) key-to-find)
			(throw 'found (cdr elem))
			)))
	  )
	))

(defun jong-http-restclient-parse-and-set-temporary ()
  (interactive)
  (setq jong-http-temporary-value (jong-http-resclient-parse-response-json-and-getkey "accTkn"))
  )


;; (defun jong-http-set-temporary-value (value)
;; (setq jong-http-temporary-value value))



(provide 'jong-http)

