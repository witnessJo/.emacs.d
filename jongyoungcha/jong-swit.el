

(defgroup jong-swit nil
  "Quickly switch current window."
  :group 'convenience
  :prefix "jong-swit")


(defvar jong-swit-base (format "%s/go/src/swit" (getenv "HOME")))
(defvar jong-swit-projects '(
														 ("swit-grpc-task-golang" "dev.v4")
														 ("swit-grpc-project-golang" "dev.v4")
														 ("swit-grpc-asset-golang" "dev")
														 ("swit-grpc-auth-golang" "dev.4")
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
	(interactive))


(defun jong-swit-checkout-base-projects()
	(interactive)
	(let (value
				project-name
				branch)
		(dolist (project jong-swit-projects nil)
			(setq project-name (nth 0 project))
			(setq branch (nth 1 project))
			(with-current-buffer (get-buffer-create (format "*%s-out*" (car project)))
				(setq default-directory (format "%s/%s" jong-swit-base project-name))
				(shell-command (format "echo %s" project-name) (current-buffer))
				(shell-command (format "git checkout %s" branch) (current-buffer))
				(shell-command "git pull" (current-buffer))
				)
			)
		)
	)

(defun jong-swit-run-base-projects ()
	(interactive)
	(let (value)
		(dolist (elt jong-swit-projects nil)
			(print elt)
			)
		)
	)



(provide 'jong-swit)
