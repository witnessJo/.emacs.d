;;; jongyoungcha's terminal settings...
;;; Code:

(defvar jong-shell-highlights nil "jongyoungcha's shell mode highlights...")

(add-hook 'term-load-hook
	  (lambda ()(define-key term-raw-map (kbd "M-x") 'nil)))

;; (use-package multi-term
;;   :ensure
;;   :bind (("C-c C-n" . multi-term-next)
;;          ("C-c C-p" . multi-term-prev)))

(use-package xterm-color
  :ensure)

(require 'ansi-color)

;; (defun jong-shell-font-lock-matcher (token)
;;   "jongyoungcha's shell font matcher..."
;;   (let ((case-fold-search nil))
;;     (re-search-forward   "\\success\\|good\\|continue\\|"
;;                          token 'no-error)))

;; (add-hook 'shell-mode-hook
;;           (lambda () (setq font-lock-defaults 'jong-shell-highlights)))

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (comint-send-input)))

(setq jong-shell-highlights
      '(("success\\|test\\|good" . font-lock-keyword-face)
        ("failed\\|fail\\|error" . font-lock-warning-face)))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook 'ansi-color-apply)
(add-hook 'shell-mode-hook (lambda ()
                             (setq font-lock-defaults '(jong-shell-highlights))))

;; (define-derived-mode jong-shell-mode shell-mode "jong-shell-mode"
;;   "Derived major mode of shell that customized by jongyoungcha"
;;   (setq font-lock-defaults '(jong-shell-highlights)))

;; (defun jong-shell ()
;;   "jongyoungcha's shell"
;;   (interactive)
;;   (shell)
;;   (jong-shell-mode t))


(provide 'jong-term)
