(require 'package)

(add-to-list 'package-archives
          '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)


;; enable line number mode
(global-linum-mode t)


;; set korean environment
(set-language-environment "Korean")
(prefer-coding-system 'utf-8)

;; replace C-j --> return
(global-set-key (kbd "RET") 'newline-and-indent)


;; smart-compile
(use-package smart-compile
  :ensure t
  :init
  :config
  :bind ([f6] . smart-compile))


;; helm
(use-package helm
  :ensure t
  :init
  (require 'helm)
  (require 'helm-config)
  (helm-mode t)
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-c x"))
  (define-key helm-map (kbd "<tab>") 'helm-excute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x C-b") 'helm-mini)
  
  :config
  :bind
  ("M-x" . helm-M-x)
  ("M-y" . helm-show-kill-ring)
  ("C-x b" . helm-mini)
  ("C-x C-f" . helm-find-files)
  ("C-c h i" . helm-semantic-or-imenu)
  ("C-c h m" . helm-man-woman)
  ("C-c h /" . helm-find)
  ("C-c h l" . helm-locate)
  ("C-c h o" . helm-occur)
  ("C-c h a" . helm-apropos)
  ("C-c h h g" . helm-info-gnus)
  ("C-c h h i" . helm-info-at-point)
  ("C-c h h r" . helm-info-emacs)
  ("C-c h <tab>" . helm-lisp-completion-at-point)
  ("C-c h b" . helm-resume)
  ("C-h SPC" . helm-all-mark-rings)
  ("C-c h r" . helm-regexp)
  ("C-c h x" . helm-register)
  ("C-c h t" . helm-top)
  ("C-c h s" . helm-surfraw)
  ("C-c h g" . helm-google-suggest)
  ("C-c h c" . helm-colors)
  ("C-c h C-," . helm-eval-expression-with-eldoc)
  ("C-c C-l" . helm-eshell-history)
  ("C-c C-l" . helm-comint-input-ring))

	 


;; helm-gtags
(use-package helm-gtags
  :ensure t
  :init
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)
  :config)


;; helm-projectile
(use-package helm-projectile
  :ensure t
  :init
  (require 'helm-projectile)
  (helm-projectile-on)
  (projectile-global-mode)
  (global-set-key (kbd "C-c p p") 'helm-projectile-switch-project)
  (global-set-key (kbd "C-c p f") 'helm-projectile-find-file)
  (global-set-key (kbd "C-c p b") 'helm-projectile-switch-to-buffer)
  (global-set-key (kbd "C-c p h") 'helm-projectile-find-other-file)
  :config)


;; slime
(use-package slime
  :ensure t
  :init
  :config
  (require 'slime))


;; yasnippet
(use-package yasnippet
  :ensure t
  :init
  (require 'yasnippet)
  (yas-global-mode 1)
  :config)


;; auto-complete
(use-package auto-complete
  :ensure t
  :defer t
  :init
  (require 'auto-complete)
  (require 'auto-complete-config)
  (require 'auto-complete-clang)
  (ac-config-default)
  (global-auto-complete-mode t)
  (auto-complete-mode t)
  (global-set-key (kbd "C-c ;") 'ac-complete-clang)
  :config
  (use-package auto-complete-clang :ensure t :defer t)
  (use-package auto-complete-clang-async :ensure t :defer t))

;; neotree
(use-package neotree
  :ensure t
  :defer t
  :init
  (require 'neotree)
  (global-set-key [f8] 'neotree-toggle)
  (setq projectile-switch-project-action 'neotree-projectile-action))


;; ggtags
(use-package ggtags
  :ensure t
  :defer t
  :init
  (require 'ggtags)
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
		(ggtags-mode 1))))
  (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
  (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
  (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
  (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
  (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
  (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)
  (define-key ggtags-mode-map (kbd "C-c g d") 'ggtags-find-definition)
  (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark))


;; highlight-symbol
(use-package highlight-symbol
  :ensure t
  :defer t
  :init
  (require 'highlight-symbol)
  (global-set-key [(control f3)] 'highlight-symbol)
  (global-set-key [f3]  'highlight-symbol-next)
  (global-set-key [(shift f3)] 'highlight-symbol-prev)
  (global-set-key [(meta f3)] 'highlight-symbol-query-replace)
  (global-set-key [(control shift f3)] 'highlight-symbol-remove-all))


;; undo-tree-mode
(use-package undo-tree
  :ensure t
  :defer t
  :init
  (require 'undo-tree)
  (global-undo-tree-mode t)
  
  (defalias 'redo 'undo-tree-redo)
  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "C-S-z") 'redo))


;; go to local variable
(defun bhj-isearch-from-bod (&optional col-indent)
  (interactive "p")
  (let ((word (current-word)))
    (beginning-of-defun)
    (setq regexp-search-ring (cons (concat "\\b" word "\\b") regexp-search))
    (search-forward-regexp (concat "\\b" word "\\b"))))



(defun bhj-isearch-from-bod (&optional col-indent)
  (interactive "p")
  (let ((word (current-word)))
    (beginning-of-defun)
    (setq regexp-search-ring (cons (concat "\\b" word "\\b") regexp-search-ring))
    (search-forward-regexp (concat "\\b" word "\\b"))))


(global-set-key (kbd "C-c g l") 'bhj-isearch-from-bod)

				    


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (undo-tree-mode highlight-symbol auto-complete-clang-async auto-complete-clang auto-complete yasnippet slime helm-projectile helm-gtags helm smart-compile use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
