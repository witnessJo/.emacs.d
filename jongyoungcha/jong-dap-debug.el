;;; Code :

(use-package dap-mode
  :ensure t
  :config
  (dap-mode t)
  (dap-ui-mode t)
	(tooltip-mode t)
	(dap-tooltip-mode t)
	(require 'dap-lldb)
	(require 'dap-node)
	(require 'dap-go)
	(require 'dap-gdb-lldb))


(defun jong-dap-debug-toggle-show-ui ()
	(interactive)
	(let ((prev-buffer-name (buffer-name)))
		(if (get 'jong-dap-debug-toggle-show-ui 'state)
				(progn
					(dap-ui-sessions)
					(dap-ui-locals)
					(switch-to-buffer prev-buffer-name)
					(put 'jong-dap-debug-toggle-show-ui 'state nil))
			(progn
				(when (get-buffer-window "*dap-ui-sessions*")
					(delete-window (get-buffer-window "*dap-ui-sessions*")))
				(when (get-buffer-window "*dap-ui-locals*")
					(delete-window (get-buffer-window "*dap-ui-locals*")))
				(put 'jong-dap-debug-toggle-show-ui 'state t))
			))
	)


(defun jong-dap-debug-setups ()
	(interactive)
	(dap-gdb-lldb-setup)
	(dap-go-setup)
	(dap-node-setup))


(defun jong-dap-debug-goto-repl()
  (interactive)
	(when (equal (get-buffer "*dap-ui-repl*") nil)
		(call-interactively 'dap-ui-repl)
		(sleep-for 0.5))
  (switch-to-buffer "*dap-ui-repl*"))

(global-set-key (kbd "<f12>") 'dap-step-out)
(global-set-key (kbd "<f11>") 'dap-step-in)
(global-set-key (kbd "<f10>") 'dap-next)
(global-set-key (kbd "<f9>") 'dap-breakpoint-toggle)
(global-set-key (kbd "<f8>") 'dap-continue)
(global-set-key (kbd "<f7>") 'jong-dap-debug-goto-repl)
(global-set-key (kbd "<f5>") 'dap-debug)
(global-set-key (kbd "<f1>") 'dap-eval-region)

(global-set-key (kbd "C-c d d") 'jong-dap-debug-toggle-show-ui)
(global-set-key (kbd "C-c d b") 'dap-ui-breakpoints)
(global-set-key (kbd "C-c d s") 'dap-ui-sessions)
(global-set-key (kbd "C-c d l") 'dap-ui-locals)
(global-set-key (kbd "C-c d <backspace>") 'dap-delete-session)
(global-set-key (kbd "C-c d k") 'dap-ui-sessions-delete-session)
(global-set-key (kbd "C-c d <return>") 'dap-ui-sessions-select)



;; (dap-register-debug-template "SwitNodeV1"
;; (list :type "node"
;; :args "-i"
;; :cwd nil
;; :env '(("DEBUG" . "1"))
;; :target-module (expand-file-name "~/go/src/swit/swit-apiv1/")
;; :request "launch"
;; :name "SwitNodev1"))




(provide 'jong-dap-debug)
