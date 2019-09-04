;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rust develope environments ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar jong-rust-output "*jong-rust-output*"
	"Output output jong-rust.")

(use-package rust-mode
	:ensure t
	:config
	(setq rust-format-on-save t))

;; (use-package racer
;; :ensure t
;; :config
;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'racer-mode-hook #'eldoc-mode)
;; (add-hook 'racer-mode-hook #'company-mode))


(defun jong-rust-install ()
	(interactive)
	(with-current-buffer (get-buffer-create jong-rust-output)
		(display-buffer jong-rust-output)
		(shell-command "curl https://sh.rustup.rs -sSf | sh" jong-rust-output jong-rust-output)))


(defun jong-rust-install-racer-bins ()
	(interactive)
	(with-current-buffer (get-buffer-create jong-rust-output)
		(display-buffer jong-rust-output)
		(shell-command "rustup toolchain add nightly" jong-rust-output jong-rust-output)
		(shell-command "rustup component add rust-src" jong-rust-output jong-rust-output)))


(defun jong-rust-install-rls-bins ()
	(interactive)
	(with-current-buffer (get-buffer-create jong-rust-output)
		(display-buffer jong-rust-output)
		(shell-command "rustup component add rls rust-analysis rust-src" jong-rust-output jong-rust-output)))

(use-package flycheck-rust
	:ensure t
	:config
	(with-eval-after-load 'rust-mode
		(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package cargo
  :ensure t)

(add-hook 'rust-mode-hook (lambda()
														(setq lsp-ui-sideline-enable nil)
														(setq lsp-ui-doc-enable nil)
														(lsp)))


(setq rust-format-on-save t)


(define-key rust-mode-map (kbd "C-c c c") #'rust-compile)
;; (define-key rust-mode-map (kbd "C-c c r") #'rust-run)
(define-key rust-mode-map (kbd "C-c r ,") #'lsp-find-references)
(define-key rust-mode-map (kbd "C-c r .") #'lsp-find-definition)
(define-key rust-mode-map (kbd "C-c r i") #'lsp-find-implementation)
(define-key rust-mode-map (kbd "C-c r r") #'lsp-rename)
(define-key rust-mode-map (kbd "C-c r l") #'helm-imenu)


(provide 'jong-rust)
