;;; jongyoungcha's terminal settings...
;;; Code:

(defvar jong-shell-highlights nil "jongyoungcha's shell mode highlights...")

(setq jong-shell-highlights
      '(("success\\|test\\|good" . font-lock-keyword-face)
        ("failed\\|fail\\|error" . font-lock-warning-face)))


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
	(eshell-send-input))
  )

(setq jong-shell-highlights
      '(("success\\|test\\|good" . font-lock-keyword-face)
        ("failed\\|fail\\|error" . font-lock-warning-face)))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; (defun jong-term-clear ()
;; (interactive)
;; (erase-buffer)
;; (comint-send-input))

;; (defun jong-term-clear-hook ()
;; (local-set-key "C-c c l" 'comint-clear-buffer))
;; (add-hook 'shell-mode-hook 'jong-term-clear-hook)

(setq comint-prompt-read-only t)
(defun jong-term-comint-preoutput-turn-buffer-read-only (text)
  (propertize text 'read-only t))


(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-to-list 'comint-output-filter-functions 'jong-term-comint-preoutput-turn-buffer-read-only)

(use-package vterm
  :ensure t
  :config
  (define-key vterm-mode-map (kbd "C-s") nil)
  (define-key vterm-mode-map (kbd "C-c C-c") 'vterm-send-C-c)
  (define-key vterm-mode-map (kbd "C-<left>") 'vterm-send-M-b)
  (define-key vterm-mode-map (kbd "C-<right>") 'vterm-send-M-f)
  (add-hook 'vterm-mode-hook (lambda ()
							   (disable-jong-keys-minor-mode)
							   ))
  )

(add-hook 'vterm-mode-hook
		  (lambda ()
			(cua-mode -1)
			(local-set-key (kbd "C-v") #'term-paste)
			))


(add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path) (setq default-directory path))))

(push (list "find-file-below"
            (lambda (path)
              (if-let* ((buf (find-file-noselect path))
                        (window (display-buffer-below-selected buf nil)))
                  (select-window window)
                (message "Failed to open file: %s" path))))
      vterm-eval-cmds)


(provide 'jong-term)
