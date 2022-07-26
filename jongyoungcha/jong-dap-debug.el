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
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)
  (setq dap-ui-variable-length 100)

  (defun disable-lsp-ui-doc (orig-fun &rest args)
    (lsp-ui-doc-enable nil))

  (defun enable-lsp-ui-doc (orig-fun &rest args)
    (lsp-ui-doc-enable t))

  (advice-add 'dap-debug :after #'disable-lsp-ui-doc)
  (advice-add 'dap-disconnect :after #'enable-lsp-ui-doc)
  
  (setq dap-auto-configure-features '(locals expressions breakpoints tooltip))

  (define-key treemacs-mode-map (kbd "D") (lambda()
                                            (interactive)
                                            (let (buffer-name (buffer-name (current-buffer)))
                                              (when buffer-name "Expressions"
                                                    (call-interactively 'dap-ui-expressions)
                                                    (call-interactively 'dap-ui-expressions-remove))
                                              )
                                            ))

  ;; (add-hook 'dap-stopped-hook
  ;; (lambda (arg)
  ;; (call-interactively #'dap-hydra)
  ;; (call-interactively #'jong-dap-go-to-output-buffer)
  ;; ))
  )

(defun jong-dap-debug-toggle-show-ui ()
  (interactive)
  (let ((prev-buffer-name (buffer-name)))
	(if (get 'jong-dap-debug-toggle-show-ui 'state)
		(progn
		  ;; (dap-ui-sessions)
		  ;; (dap-ui-locals)
		  ;; (dap-go-to-output-buffer)
		  (dap-ui-show-many-windows)
		  (switch-to-buffer prev-buffer-name)
		  ;; (jong-dap-go-to-output-buffer)
		  ;; (when (get-buffer-window "*dap-ui-sessions*")
		  ;; (select-window (get-buffer-window "*dap-ui-sessions*"))
		  ;; (jong-common-enlarge-window-horizontally-10))
		  (put 'jong-dap-debug-toggle-show-ui 'state nil))
	  (progn
		;; (when (get-buffer-window "*dap-ui-sessions*")
		;; (delete-window (get-buffer-window "*dap-ui-sessions*")))
		;; (when (get-buffer-window "*dap-ui-locals*")
		;; (delete-window (get-buffer-window "*dap-ui-locals*")))
		(dap-ui-hide-many-windows)
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


(defun jong-dap-go-to-output-buffer ()
  (interactive)
  (let ((prev-window (selected-window)))
	(progn
      
	  ;; (dap-go-to-output-buffer)
	  ;; (enlarge-window 10)
	  ;; (compilation-mode)
	  ;; (select-window prev-window)
      )
	)
  )


(provide 'jong-dap-debug)
