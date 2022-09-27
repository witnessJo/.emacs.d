;;; Code:

;; (dap-register-debug-template "SwitNodeV1"
;; (list :type "node"
;; :args "-i"
;; :cwd nil
;; :env '(("DEBUG" . "1"))
;; :target-module (expand-file-name "~/go/src/swit/swit-apiv1/")
;; :request "launch"
;; :name "SwitNodev1"))

;; (dap-register-debug-template "SwitFileGo"
;; (list :type "go"
;; :args "-i"
;; :env '(
;; ("BUCKET_NAME" . "test-swit")
;; ("FILE_SERVICE_PORT" . "10040")
;; ("SWIT_FILE_LOG_LEVEL" ."debug")
;; ("GOOGLE_STORAGE_CREDENTIALS" . "/Users/swit-mac/swit-gke-resource-access.json")
;; )
;; :program "/Users/swit-mac/go/src/swit/swit-gcs-file-golang/server"
;; :request "launch"
;; :mode "exec"
;; :name  "SwitFileGo"))


(defvar jong-debug-setting-path (format "%s/.emacs.d/jongyoungcha/jong-debug-settings.el" (getenv "HOME")))

(defun jong-debug-setting-toggle-open-file()
  "Open the dap-debug setting file."
  (interactive)
  (if (get-buffer "jong-debug-settings.el")
	  (kill-buffer (get-buffer "jong-debug-settings.el"))
	(find-file-at-point jong-debug-setting-path))
  )

(dap-register-debug-template "koscomtls"
							 (list :type "go"
								   :args "call -u https://naver.com -X POST -d \'test message\'"
								   :env '(
										  ("MTLS_SENTBIZ_CRT_PATH" . "/Users/richard/koscom_dev_sentbe_com.crt")
										  ("MTLS_SENTBIZ_KEY_PATH" . "/Users/richard/koscom.dev.sentbe.com.key")
										  ("MTLS_SENTBIZ_FULL_CHAIN_PATH" . "/Users/richard/koscom_dev_sentbe_com.pem")
										  )
								   :program "/Users/richard/go/src/demeter/cmd/koscommtls/main.go"
								   :envFile nil
								   :buildFlags "-gcflags '-N -l'"
								   :request "launch"
								   :mode "debug"
								   :name  "koscomtls"))


(dap-register-debug-template "demter"
							 (list :type "go"
								   :args "start develop"
								   :env '(
										  ;; ("MTLS_SENTBIZ_CRT_PATH" . "/Users/richard/koscom_dev_sentbe_com.crt")
										  ;; ("MTLS_SENTBIZ_KEY_PATH" . "/Users/richard/koscom.dev.sentbe.com.key")
										  ;; ("MTLS_SENTBIZ_FULL_CHAIN_PATH" . "/Users/richard/koscom_dev_sentbe_com.pem")
										  )
								   :program "/Users/richard/go/src/demeter/main.go"
								   :envFile nil
								   :buildFlags "-gcflags '-N -l'"
								   :request "launch"
								   :mode "debug"))


(dap-register-debug-template "maat"
							 (list :type "go"
								   :request "launch"
								   :name "maat"
								   :args "start grpc"
								   :buildFlags "-gcflags '-N -l'"
								   :env '(
										  ("TARGET" . "develop")
										  )
								   :program "/Users/richard/go/src/maat/maat"
								   :envFile nil
								   :mode "exec"))

(dap-register-debug-template "geth"
							 (list :type "go"
								   :request "launch"
								   :name "geth"
								   :args ""
                                   :buildFlags "-gcflags '-N -l'"
								   :env '(
                                          ("GO111MODULE" . "on")
										  )
								   :program "/Users/jongyoungcha/gowork/go-ethereum/build/bin/geth"
								   :envFile nil
								   :mode "exec"))



(dap-register-debug-template "fbs"
							 (list :type "go"
								   :request "launch"
								   :name "fbs"
								   :args "start develop"
								   :buildFlags "-gcflags '-N -l'"
								   :env '(
										  ;; ("ENV_MODE" . "")
										  ;; ("MTLS_SENTBIZ_KEY_PATH" . "/Users/richard/koscom.dev.sentbe.com.key")
										  ;; ("MTLS_SENTBIZ_FULL_CHAIN_PATH" . "/Users/richard/koscom_dev_sentbe_com.pem")
										  )
								   :program "/Users/richard/go/src/fbs/fbs"
								   :envFile nil
								   :mode "exec"))



(dap-register-debug-template "dbconnection"
							 (list :type "go"
								   :args ""
								   :env '(
										  ("DB_HOST" . "127.0.0.1")
										  ("DB_PORT" . "5432")
										  ("DB_USER" . "postgres")
										  ("DB_PASSWD" . "sentbe1234.")
										  )
								   :program "/Users/richard/go/src/dbconnection/cmd/main.go"
								   :envFile nil
								   :buildFlags "-gcflags '-N -l'"
								   :request "launch"
								   :mode "debug"))



(dap-register-debug-template "bc-backend"
							 (list :type "go"
								   :request "launch"
								   :name "bc-backend"
								   :args ""
								   :buildFlags "-gcflags '-N -l'"
								   :env '(
										  ("TARGET" . "develop")
										  )
                                   :default-directory "/Users/jongyoungcha/gowork/mdl-manager/services/bc-backend/"
								   :program "/Users/jongyoungcha/gowork/mdl-manager/services/bc-backend/main.go"
								   :envFile "/Users/jongyoungcha/gowork/mdl-manager/services/bc-backend/configs/.env"
								   :mode "debug"))


(defun jong-debug-go-debug-current-test ()
  (interactive)
  (let ((name (go-test--get-current-test)))
	(dap-debug
	 (list :type "go"
		   :request "launch"
		   :name (concat "Go: Debug " name " test")
		   :mode "auto"
		   :program "${fileDirname}"
		   :buildFlags "-gcflags '-N -l'"
		   :args (concat "-test.run ^" name "$")
		   :env
           '(("DB_ADDRESS" . "localhost")
            ("DB_PORT" . "20306")
            ("DB_NAME" . "mdl_manager")
            ("DB_USERNAME" . "root")
            ("DB_PASSWORD" . "root")
            ("DB_DRIVER" . "mysql")
            ("DB_TABLES" . "caservers,causers,orderers,peers,msps,organizations,channels,channel_joined_peers,channel_msps,chaincodes,chaincodes_funcs,chaincodes_orderers,chaincodes_peers,node_ports,nodes")
            ("DB_LOGGING" . "yes")

            ("REDIS_ADDRESS" . redis)
            ("REDIS_PORT" . 6379)
            ("REDIS_PASSWORD" . ""))

		   :envFile nil))))



(provide 'jong-debug-settings)
