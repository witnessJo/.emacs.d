;;; Code:

(defgroup jong-bash nil
  "Jongyoungcha bash."
  :prefix "jong-bash-"
  :group 'jong-bash)

(defconst jong-bash-output-buffer "*jong-bash-output*")
(defcustom jong-bash-command "bashdb"
  "Command for bashdb.")


(use-package bash-completion
  :ensure t
  :config
  (autoload 'bash-completion-dynamic-complete
    "bash-completion"
    "BASH completion hook")
  (add-hook 'shell-dynamic-complete-functions
			'bash-completion-dynamic-complete))


(defun jong-bash-bashdb ()
  (interactive)
  (let ((ret (car (jong-common-process-exit-code-and-output "which" "bashdb"))))
    (if (equal ret 0)
		(with-current-buffer (get-buffer-create jong-bash-output-buffer)
		  (display-buffer (current-buffer))
		  (gud-mode)
		  (setq jong-bash-command (read-string "bashdb command : " jong-bash-command))
		  (async-shell-command jong-bash-command jong-bash-output-buffer jong-bash-output-buffer)
		  (message "Couldnt find \"bashdb\""))
	  (message "Couldnt find the bashdb binary..."))
    )
  )

(defun jong-bash-send-command ()
  (interactive))


(provide 'jong-bash)
