;;;Code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  python develope environments  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package elpy
  :ensure t)
(with-eval-after-load 'elpy
  (require 'elpy)
  (elpy-enable)
  ;; (elpy-use-ipython)
  (setq elpy-rpc-backend "jedi")
  (setq python-shell-interpreter "ipython")
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  (add-to-list 'python-shell-completion-native-disabled-interpreters "ipython")
  (setq jedi:complete-on-dot t)
  (setq jedi:environment-root "jedi"))

(use-package company-jedi
  :ensure t)
(add-hook 'python-mode-hook
		      (lambda()
			      (add-to-list 'company-backend 'company-jedi)))

(global-set-key (kbd "C-c i") 'indent-region)



(use-package anaconda-mode
  :ensure t)
(require 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
;; (add-hook 'python-mode-hook 'anaconda-eldoc-mode)


(add-hook 'python-mode-hook
		      (lambda ()
			      (local-set-key (kbd "C-c g g") 'anaconda-mode-find-definitions)
			      (local-set-key (kbd "C-c c c") 'jyc-run-python)
			      (local-set-key (kbd "C-S-g") 'close-compilation-window)
			      (linum-mode t)
			      ))


(provide 'jong-python)
