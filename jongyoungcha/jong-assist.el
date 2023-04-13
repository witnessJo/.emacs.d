


;; (use-package company-tabnine
;; :ensure t
;; )

(require 'cl)
;; (let ((pkg-list '(use-package
;; s
;; dash
;; editorconfig
;; company)))
;; (package-initialize)
;; (when-let ((to-install (map-filter (lambda (pkg _) (not (package-installed-p pkg))) pkg-list)))
;; (package-refresh-contents)
;; (mapc (lambda (pkg) (package-install pkg)) pkg-list)))

(use-package editorconfig
  :ensure t)

(use-package s
  :ensure t)

(use-package dash
  :ensure t)

(use-package company
  :ensure t)

(use-package copilot
  :load-path (lambda ()
               (expand-file-name "copilot.el" user-emacs-directory))
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-p") 'copilot-previous-completion)
  (define-key copilot-completion-map (kbd "C-n") 'copilot-next-completion)
  ;; don't show in mode line
  :diminish)

(defun rk/no-copilot-mode ()
  "Helper for `rk/no-copilot-modes'."
  (copilot-mode -1))

(defvar rk/no-copilot-modes '(shell-mode
                              inferior-python-mode
                              eshell-mode
                              term-mode
                              vterm-mode
                              comint-mode
                              compilation-mode
                              debugger-mode
                              dired-mode-hook
                              compilation-mode-hook
                              flutter-mode-hook
                              minibuffer-mode-hook)
  "Modes in which copilot is inconvenient.")

(add-hook 'prog-mode-hook 'copilot-mode)
(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))


(defun my/copilot-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (indent-for-tab-command)))
;; (with-eval-after-load 'copilot
;; (define-key copilot-mode-map (kbd "<tab>") #'my/copilot-tab))

;; (defun rk/copilot-disable-predicate ()
;;   "When copilot should not automatically show completions."
;;   (or rk/copilot-manual-mode
;;       (member major-mode rk/no-copilot-modes)
;;       (company--active-p)))

;; (defvar rk/copilot-manual-mode nil
;;   "When `t' will only show completions when manually triggered, e.g. via M-C-<return>.")

;; (defun rk/copilot-change-activation ()
;;   "Switch between three activation modes:
;; - automatic: copilot will automatically overlay completions
;; - manual: you need to press a key (M-C-<return>) to trigger completions
;; - off: copilot is completely disabled."
;;   (interactive)
;;   (if (and copilot-mode rk/copilot-manual-mode)
;;       (progn
;;         (message "deactivating copilot")
;;         (global-copilot-mode -1)
;;         (setq rk/copilot-manual-mode nil))
;;     (if copilot-mode
;;         (progn
;;           (message "activating copilot manual mode")
;;           (setq rk/copilot-manual-mode t))
;;       (message "activating copilot mode")
;;       (global-copilot-mode))))

;; (define-key global-map (kbd "M-C-<escape>") #'rk/copilot-change-activation)

;; (defun rk/copilot-complete-or-accept ()
;;   "Command that either triggers a completion or accepts one if one
;; is available. Useful if you tend to hammer your keys like I do."
;;   (interactive)
;;   (if (copilot--overlay-visible)
;;       (progn
;;         (copilot-accept-completion)
;;         (open-line 1)
;;         (next-line))
;;     (copilot-complete)))

;; (define-key copilot-mode-map (kbd "M-C-<next>") #'copilot-next-completion)
;; (define-key copilot-mode-map (kbd "M-C-<prior>") #'copilot-previous-completion)
;; (define-key copilot-mode-map (kbd "M-C-<right>") #'copilot-accept-completion-by-word)
;; (define-key copilot-mode-map (kbd "M-C-<down>") #'copilot-accept-completion-by-line)
;; (define-key global-map (kbd "M-C-<return>") #'rk/copilot-complete-or-accept)

;; (defun rk/copilot-tab ()
;;   "Tab command that will complet with copilot if a completion is
;; available. Otherwise will try company, yasnippet or normal
;; tab-indent."
;;   (interactive)
;;   (or (copilot-accept-completion)
;;       (company-yasnippet-or-completion)
;;       (indent-for-tab-command)))

;; (define-key global-map (kbd "<tab>") #'rk/copilot-tab)

;; (defun rk/copilot-quit ()
;;   "Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is
;; cleared, make sure the overlay doesn't come back too soon."
;;   (interactive)
;;   (condition-case err
;;       (when copilot--overlay
;;         (lexical-let ((pre-copilot-disable-predicates copilot-disable-predicates))
;;           (setq copilot-disable-predicates (list (lambda () t)))
;;           (copilot-clear-overlay)
;;           (run-with-idle-timer
;;            1.0
;;            nil
;;            (lambda ()
;;              (setq copilot-disable-predicates pre-copilot-disable-predicates)))))
;;     (error handler)))

;; (advice-add 'keyboard-quit :before #'rk/copilot-quit)

(provide 'jong-assist)
