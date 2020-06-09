

(defgroup jong-swit nil
  "Quickly switch current window."
  :group 'convenience
  :prefix "jong-swit")


(defvar jong-swit-base (format "%s/go/src/swit" (getenv "HOME")))
(defvar jong-swit-projects '(
							 ("swit-grpc-task-golang" "dev.v4" "./server")
							 ("swit-grpc-project-golang" "dev.v4" "./server")
							 ("swit-grpc-asset-golang" "dev" "./server")
							 ("swit-grpc-auth-golang" "dev.4" "./server")
							 ("swit-grpc-message-golang" "dev.v4" "./server")
							 ))

(defun jong-swit-set-local-envs ()
  (interactive)
  (setenv  "SWIT_MYSQL_USERNAME" "switapp")
  (setenv  "SWIT_MYSQL_PASSWORD" "switapp0801!")
  (setenv  "SWIT_MYSQL_IP" "35.233.206.64")
  (setenv  "SWIT_MYSQL_PORT" "3306")
  (setenv  "SWIT_MYSQL_DATABASE" "swit")
  (setenv  "SWIT_RUN_MODE" "debug")
  (setenv  "ACTIVITY_PORT" "50074")
  (setenv  "SWIT_REDIS_ADDR" "10.6.0.3:6379")
  (setenv  "SWIT_REDIS_PASSWORD" "")
  (setenv  "ELASTIC_SERVICE_ADDR" "10.0.0.35:50071")
  (setenv  "WORKSPACE_SERVICE_ADDR" "10.0.0.35:50052")
  (setenv  "GOOGLE_APPLICATION_CREDENTIALS" "/Users/swit-mac/swit-gke-resource-access.json")
  (setenv  "PUB_SUB_CLIENT" "swit-dev")
  )

(defun jong-swit-set-dev-envs ()
  (interactive)
  ()
  )

(defun jong-swit-clone-base-project()
  (interactive)
  
  )


(defun jong-swit-checkout-base-projects()
  (interactive)
  (let (value
		project-name
		branch
		target-buffer-name)
	(setq target-buffer-name "*jong-swit-checkout-base-projects*")
	(when (get-buffer target-buffer-name)
	  (kill-buffer target-buffer-name))

	(dolist (project jong-swit-projects nil)
	  (setq project-name (nth 0 project))
	  (setq branch (nth 1 project))
	  (with-current-buffer (get-buffer-create target-buffer-name)
		(display-buffer (current-buffer))
		(setq default-directory (format "%s/%s" jong-swit-base project-name))
		(goto-char (point))
		(insert ">>>" project-name "\n")
		(ignore-errors
		  (set-process-sentinel
		   (start-process (format "git-checkout/%s" project-name) (current-buffer) "git" "checkout" branch)
		   (lambda (p e)
			 (start-process (process-name p) (get-buffer target-buffer-name) "git" "pull"))
		   ))
		)
	  )
	)
  )


(defun jong-swit-run-base-projects ()
  (interactive)
  (let (value
		project-name
		branch
		target-buffer-name
		swit-base-process)
	
	(dolist (project jong-swit-projects nil)
	  
	  (setq project-name (nth 0 project))
	  (setq branch (nth 1 project))
	  (setq target-buffer-name (format "*%s/%s*" (nth 0 project) (nth 1 project)))
	  
	  (with-current-buffer (get-buffer-create target-buffer-name)
		(ansi-color-for-comint-mode-on)
		(comint-mode)
		(setq default-directory (format "%s/%s" jong-swit-base project-name))
		(display-buffer (current-buffer))
		(setq swit-base-process (start-process-shell-command "server" (current-buffer) "./server"))
		(set-process-filter swit-base-process 'comint-output-filter)
		)
	  )
	)
  )

(require 'ansi-color)
(defun display-ansi-colors ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))


(provide 'jong-swit)
