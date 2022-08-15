
;; (add-hook 'yaml-mode-hook (lambda()
;; (local-set-key (kbd "C-M-\\") 'yaml-indent-line)
;; ))

(defun jong-yaml-indent-left ()
  "Do indentation of left direction."
  (interactive)
  (jong-cusor-indent -2)
)

(defun jong-yaml-indent-right ()  
  "Do indentation of right direction."
  (interactive)
  (jong-cusor-indent 2)
)

(define-key yaml-mode-map (kbd "C-M-\\") 'yaml-indent-line)
(define-key yaml-mode-map (kbd "C-<") 'jong-yaml-indent-left)
(define-key yaml-mode-map (kbd "C->") 'jong-yaml-indent-right)

(provide 'jong-yaml)
