;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rust develope environments ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar jong-rust-output "*jong-rust-output*"
	"Output output jong-rust.")

(use-package rust-mode
  :ensure t)

(use-package racer
  :ensure t
	:config
	(add-hook 'rust-mode-hook #'racer-mode)
	(add-hook 'racer-mode-hook #'eldoc-mode)
	(add-hook 'racer-mode-hook #'company-mode))

(defun jong-rust-install-racer-bins ()
	(interactive)
	(progn
		(display-buffer jong-rust-output)
		 (shell-command "rustup toolchain add nightly" jong-rust-output jong-rust-output)
		 (shell-command "rustup component add rust-src" jong-rust-output jong-rust-output)))

(use-package flycheck-rust
	:ensure t
	:config
	(with-eval-after-load 'rust-mode
		(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package cargo
  :ensure t)

(add-hook 'rust-mode-hook #'lsp)


(setq rust-format-on-save t)


(define-key rust-mode-map (kbd "C-c c c") #'rust-compile)
(define-key rust-mode-map (kbd "C-c c r") #'rust-run)


(provide 'jong-rust)
