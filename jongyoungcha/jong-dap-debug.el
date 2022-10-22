;;; Code :
(use-package dap-mode
  :ensure t
  :config
  (require 'dap-ui)
  (require 'dap-lldb)
  (require 'dap-node)
  (require 'dap-dlv-go)
  (require 'dap-gdb-lldb)
  
  (dap-mode 1)
  (dap-ui-mode 1)
  (tooltip-mode 1)
  (dap-tooltip-mode 1)
  (dap-ui-controls-mode 1)
  
  (setq left-fringe-width 16)
  (set-fringe-style (quote (12 . 8)))
  (setq dap-print-io t)
  (setq dap-mouse-popup-timeout 1)
  (setq dap-ui-variable-length 100)
  (setq dap-tooltip-mouse-motions-active t)
  
  (setq dap-auto-configure-features '(locals expressions breakpoints tooltip))
  )



;; (add-hook 'dap-tooltip-mode-hook (lambda() (sleep-for 3000)))
;; (defun jong-dap-debug)

;; (defun jong-dap-debug-toggle-show-ui ()
;; (interactive)
;; (let ((prev-buffer-name (buffer-name)))
;; (if (get 'jong-dap-debug-toggle-show-ui 'state)
;; (progn
;; (dap-ui-sessions)
;; (dap-ui-locals)
;; (dap-go-to-output-buffer)
;; (dap-ui-show-many-windows)
;; (switch-to-buffer prev-buffer-name)
;; (jong-dap-go-to-output-buffer)
;; (when (get-buffer-window "*dap-ui-sessions*")
;; (select-window (get-buffer-window "*dap-ui-sessions*"))
;; (jong-common-enlarge-window-horizontally-10))
;; (put 'jong-dap-debug-toggle-show-ui 'state nil))
;; (progn
;; (when (get-buffer-window "*dap-ui-sessions*")
;; (delete-window (get-buffer-window "*dap-ui-sessions*")))
;; (when (get-buffer-window "*dap-ui-locals*")
;; (delete-window (get-buffer-window "*dap-ui-locals*")))
;; (dap-ui-hide-many-windows)
;; (put 'jong-dap-debug-toggle-show-ui 'state t))
;; ))
;; )


;; (defun jong-dap-debug-setups ()
;; (interactive)
;; (dap-gdb-lldb-setup)
;; (dap-go-setup)
;; (dap-node-setup))


;; (defun jong-dap-debug-goto-repl()
;; (interactive)
;; (when (equal (get-buffer "*dap-ui-repl*") nil)
;; (call-interactively 'dap-ui-repl)
;; (sleep-for 0.5))
;; (switch-to-buffer "*dap-ui-repl*"))


;; (defun jong-dap-go-to-output-buffer ()
;; (interactive)
;; (let ((prev-window (selected-window)))
;; (progn

;; (dap-go-to-output-buffer)
;; (enlarge-window 10)
;; (compilation-mode)
;; (select-window prev-window)
;; )
;; )
;; )


(provide 'jong-dap-debug)
