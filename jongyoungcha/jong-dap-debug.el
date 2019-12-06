;;; Code :

(use-package dap-mode
  :ensure t
  :config
  (dap-mode t)
  (dap-ui-mode t)
	(require 'dap-lldb))

(use-package dap-mode
	:ensure t)

(defconst jong-dap-repl-buffer-name "*dap-ui-repl*")

(defun jong-dap-debug-goto-repl()
  (interactive)
  (switch-to-buffer "*dap-ui-repl*"))

(global-set-key (kbd "<f12>") 'dap-step-out)
(global-set-key (kbd "<f11>") 'dap-step-in)
(global-set-key (kbd "<f10>") 'dap-next)
(global-set-key (kbd "<f9>") 'dap-breakpoint-toggle)
(global-set-key (kbd "<f8>") 'dap-continue)
(global-set-key (kbd "<f7>") 'jong-dap-debug-goto-repl)
(global-set-key (kbd "<f5>") 'dap-debug)
(global-set-key (kbd "<f1>") 'dap-eval-region)


(provide 'jong-dap-debug)
