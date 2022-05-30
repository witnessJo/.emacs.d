
(use-package org
  :ensure t
  :config
;;; image
  (setq org-startup-with-inline-images t))

;;; plantuml
(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)))
(setq org-confirm-babel-evaluate nil)
(setq org-plantuml-jar-path
      (expand-file-name "~/Downloads/plantuml.jar"))
(add-hook 'org-babel-after-execute-hook
          (lambda ()
            (when org-inline-image-overlays
              (org-redisplay-inline-images))))
(add-to-list 'org-structure-template-alist
             '("u" "#+BEGIN_SRC plantuml :file ?.png
                    \nskinparam monochrome true
                    \n#+END_SRC"))


(add-hook 'org-mode-hook (lambda ()
						   (setq truncate-lines t)
						   (setq fill-column 80)
						   (turn-on-auto-fill)
						   (local-set-key (kbd "C-c e h") 'org-hugo-export-to-md)
                           (local-set-key (kbd "C-c e m") 'org-pandoc-export-to-markdown)
                           (local-set-key (kbd "C-c e p") 'org-pandoc-export-to-latex-pdf)
                           (local-set-key (kbd "C-c e d") 'org-pandoc-export-to-docx)
						   (local-set-key (kbd "C-S-<up>") 'jong-cursor-move-text-up)
						   (local-set-key (kbd "C-S-<down>") 'jong-cursor-move-text-down)
                           (local-set-key (kbd "C-'") 'jong-avy-goto-word-1)
                           )
          )




(defun jong-org-insert-src-block ()
  (interactive)
  (insert "#+BEGIN_SRC \n\n#+END_SRC")
  )

(use-package pandoc
  :ensure t)

(use-package ox-pandoc
  :ensure t)

(use-package verb
  :ensure t)

(use-package ox-hugo
  :ensure t)

(provide 'jong-org)
