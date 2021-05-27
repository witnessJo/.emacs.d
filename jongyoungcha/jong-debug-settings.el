

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




(provide 'jong-debug-settings)
