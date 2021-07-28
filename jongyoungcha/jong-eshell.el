;;; Code


(defvar jong-eshell-keywords '(("error\\|failed\\|debug\\|" . font-lock-warning-face)
							   "Keywords for jong-eshll-minor-mode highlighting"))

(add-hook 'eshell-mode-hook (lambda()
							  (font-lock-add-keywords nil jong-eshell-keywords)))



(provide 'jong-eshell)
