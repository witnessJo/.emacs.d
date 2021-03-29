(use-package helm
  :ensure t
  :init
  :config
  (setq helm-split-window-in-side-p t)
  (helm-mode 1)
  (setq helm-candidate-number-limit 500)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "C-x r l") 'helm-bookmarks)
  
  (define-key helm-find-files-map (kbd "M-<up>") 'helm-previous-page)
  (define-key helm-find-files-map (kbd "M-<down>") 'helm-next-page)
  (define-key helm-find-files-map (kbd "M-<right>") 'helm-execute-persistent-action)
  (define-key helm-find-files-map (kbd "M-<left>") 'helm-find-files-up-one-level)
  (define-key helm-read-file-map (kbd "M-<right>") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "M-<left>") 'helm-find-files-up-one-level))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; you need to install ag binary      ;;
;; $ brew install the_silver_searcher ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helm-ag
  :ensure t)
(with-eval-after-load 'helm-ag
  (global-set-key (kbd "C-c a g") 'helm-do-ag))

(global-set-key (kbd "C-c b") 'helm-buffers-list)
(global-set-key (kbd "C-c C-b") 'helm-buffers-list)


(use-package helm-projectile
  :ensure t)

(with-eval-after-load 'helm-projectile
  (setq helm-projectile-fuzzy-match nil))

(setq projectile-completion-system 'helm)
(helm-projectile-on)

(provide 'yc-helm.el)
