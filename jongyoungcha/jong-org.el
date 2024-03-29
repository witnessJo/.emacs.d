﻿
(use-package org
  :ensure t
  :bind (
         :map org-mode-map
         ("C-<return>" . jong-window-toggle-maximize-buffer)
         ("C-c RET" . nil)
         )
  :config
  (auto-fill-mode 1)
  (setq truncate-lines t)
  (setq org-startup-with-inline-images t))

(use-package org-roam
  :ensure t
  :after org
  :init
  (setq org-roam-v2-ack t
        org-directory (file-truename "~/jongyoungcha/org")
        org-roam-directory (file-truename "~/jongyoungcha/org/roam")
        org-roam-completion-everywhere t)
  :custom
  (org-roam-directory (file-truename org-directory)))

(use-package org-download
  :ensure t
  :init
  :config
  (add-hook 'dired-mode-hook 'org-download-enable)
  (unless (executable-find "pngpaste")
    (cond ((string-equal system-type "darwin")
           (async-shell-command "brew install pngpaste"))
          ((string-equal system-type "gnu/linux"))
          )
    )
  )

(use-package org-ai
  :ensure
  :commands (org-ai-mode)
  :custom
  (org-ai-openai-api-token (getenv "GPT_KEY"))
  :init
  (add-hook 'org-mode-hook #'org-ai-mode)
  :config
  ;; if you are using yasnippet and want `ai` snippets
  ;; (org-ai-install-yasnippets)
  )

;;; plantuml
(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)))
(setq org-confirm-babel-evaluate nil)
(setq org-plantuml-jar-path
      (expand-file-name "~/jongyoungcha/plantuml-1.2023.1.jar"))
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
                           (local-set-key (kbd "C-c e h") 'org-hugo-export-to-md)
                           (local-set-key (kbd "C-c e m") 'org-pandoc-export-to-markdown)
                           (local-set-key (kbd "C-c e p") 'org-pandoc-export-to-latex-pdf)
                           (local-set-key (kbd "C-c e d") 'org-pandoc-export-to-docx)
						   (local-set-key (kbd "C-S-<up>") 'jong-cursor-move-text-up)
						   (local-set-key (kbd "C-S-<down>") 'jong-cursor-move-text-down)
                           (local-set-key (kbd "C-'") 'jong-avy-goto-word-1)
                           (local-set-key (kbd "<tab>") 'org-indent-item)
                           (local-set-key (kbd "S-<tab>") 'org-outdent-item)
                           (local-set-key (kbd "C-<up>") 'backward-paragraph)
                           (local-set-key (kbd "C-<down>") 'forward-paragraph)
                           (local-set-key (kbd "C-<down>") 'forward-paragraph)
                           (local-set-key (kbd "C-j") 'org-insert-item)
                           (local-set-key (kbd "C-c f f") 'org-fold-hide-entry)
                           (local-set-key (kbd "C-c f u") 'org-fold-show-entry)
                           (local-set-key (kbd "C-M-t") 'nil)
                           (local-set-key (kbd "C-M-y") 'org-download-clipboard)
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
