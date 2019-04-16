;;; Code:

(use-package restclient
  :ensure t)



(define-key restclient-mode-map (kbd "C-c C-c") (lambda()
												  (interactive)
												  (restclient-http-send-current nil t)))

(provide 'jong-http)

