


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
	;; (setenv  "GOOGLE_APPLICATION_CREDENTIALS" "/credentials/swit-gke-resource-access.json")
	(setenv  "GOOGLE_APPLICATION_CREDENTIALS" "/Users/swit-mac/swit-gke-resource-access.json")
	(setenv  "PUB_SUB_CLIENT" "swit-dev")
	)

(defun jong-swit-set-dev-envs ()
	(interactive)
	)





(provide 'jong-swit)
