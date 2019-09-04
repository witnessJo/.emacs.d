;;; Code :

(use-package dap-mode
  :ensure t
  :config
  (dap-mode t)
  (dap-ui-mode t)
	(require 'dap-lldb))

(defconst jong-dap-repl-buffer-name "*dap-ui-repl*")


(defun jong-dap-debug-goto-repl()
  (interactive)
  (jong-common-find-buffer-and-move jong-dap-repl-buffer-name))




;; (define-key dap-mode-map (kbd "<f8>") (lambda()
;; (interactive)
;; (other-window 1)
;; (call-interactively 'dap-continue)
;; (jong-common-find-buffer-and-move jong-dap-repl-buffer-name)))

;; (define-key dap-mode-map (kbd "<f9>") (lambda()
;; (interactive)
;; (other-window 1)
;; (call-interactively 'dap-breakpoint-toggle)
;; (jong-common-find-buffer-and-move jong-dap-repl-buffer-name)))

;; (define-key dap-mode-map (kbd "<f10>") (lambda()
;; (interactive)
;; (other-window 1)
;; (call-interactively 'dap-next)
;; (jong-common-find-buffer-and-move jong-dap-repl-buffer-name)))

;; (define-key dap-mode-map (kbd "<f11>") (lambda()
;; (interactive)
;; (other-window 1)
;; (call-interactively 'dap-step-in)
;; (jong-common-find-buffer-and-move jong-dap-repl-buffer-name)))

;; (define-key dap-mode-map (kbd "<f12>") (lambda()
;; (interactive)
;; (other-window 1)
;; (call-interactively 'dap-step-out)
;; (jong-common-find-buffer-and-move jong-dap-repl-buffer-name)))

(defun jong-dap-debug-set-screen()
  (interactive)
  
  )

(provide 'jong-dap-debug)
