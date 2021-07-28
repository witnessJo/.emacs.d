

(add-hook 'yaml-mode-hook (lambda()
							(local-set-key (kbd "C-M-\\") 'yaml-indent-line)
							))


(provide 'jong-yaml)
