
(use-package org
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda ()
							 (setq truncate-lines t)
							 (setq fill-column 80)
							 (turn-on-auto-fill)
							 (local-set-key (kbd "C-c e m") 'org-hugo-export-to-md)
							 (local-set-key (kbd "C-S-<up>") 'jong-cursor-move-text-up)
							 (local-set-key (kbd "C-S-<down>") 'jong-cursor-move-text-down)
                             )
			)
  )

(use-package verb
  :ensure t)

(use-package ox-hugo
  :ensure t)

(provide 'jong-org)
