
(use-package org
  :ensure t
  :bind (
		 ("C-c e m" . org-hugo-export-to-md)
		 )
  :config
  (add-hook 'org-mode-hook (lambda () (setq truncate-lines t)))
  (add-hook 'org-mode-hook (lambda () (setq fill-column 80)))
  (add-hook 'org-mode-hook (lambda ()
							 ;; (setq org-export-with-archived-trees nil)
							 ;; (setq org-export-with-broken-links 'mark)
							 ))
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  )

(use-package verb
  :ensure t)

(use-package ox-hugo
  :ensure t)

(provide 'jong-org)
