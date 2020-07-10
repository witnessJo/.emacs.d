;; (use-package dotenv
;; :ensure t
;; :config
;; (dotenv :repo "pkulev/dotenv.el"
;; :fetcher github :upgrade t))



(require 'dotenv-mode)
(add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode))




(defun jong-dotenv-load-dotenv()
  (interactive)
  )
