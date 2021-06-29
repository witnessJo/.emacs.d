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

(defun jong-debug-setting-open-file()
  "Open the dap-debug setting file."
  (interactive)
  (find-file-at-point jong-debug-setting-path)
  )

(dap-register-debug-template "koscomtls"
							 (list :type "go"
								   :args "test"
								   :env '(
										  ("GOLANG_PROTOBUF_REGISTRATION_CONFLICT" . "warn")
										  )
								   :program "/Users/richard/go/src/demeter/cmd/koscommtls/main"
								   :request "launch"
								   :mode "exec"
								   :name  "chandra"))





(provide 'jong-debug-settings)
