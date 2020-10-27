
;;; Code:

(setenv "PORT" "9090")
(setenv "NODE_ENV" "default")

(setenv "WORKSPACE_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "USER_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "CHANNEL_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "PROJECT_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "PAYMENT_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "AUTH_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "EMAIL_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "ELASTIC_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "ACTIVITY_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "MENTION_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "SAML_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "PAY_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "APPVERSION_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "SUPPORT_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "SFDC_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "MBOX_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "IMPORT_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "OVERVIEW_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "MEMBER_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "EXPORT_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "MESSAGE_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "STATS_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "IMPORT_SQL_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "TASK_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "BATCH_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "ASSET_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "COUPON_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "MISC_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "OAUTH_OPENAPI_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "OAUTH_CLIENT_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "STORE_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "SAMPLE_SERVICE_ADDR" "grpc.swit.dev:8060")
(setenv "ELK_SERVER" "10.0.0.39")

;; db information

(setenv "SWIT_APPS_MYSQL_USERNAME" "richard")
(setenv "SWIT_APPS_MYSQL_PASSWORD" "kLcyf9IVJfw")
(setenv "SWIT_APPS_MYSQL_IP" "127.0.0.1")
(setenv "SWIT_APPS_MYSQL_PORT" "3305")
(setenv "SWIT_APPS_MYSQL_DATABASE" "swit_store")

(setenv "SWIT_MYSQL_USERNAME" "richard")
(setenv "SWIT_MYSQL_PASSWORD" "kLcyf9IVJfw")
(setenv "SWIT_MYSQL_IP" "127.0.0.1")
(setenv "SWIT_MYSQL_DATABASE" "swit")

(setenv "SWIT_SUPPORT_MYSQL_IP" "127.0.0.1")
(setenv "SWIT_SUPPORT_MYSQL_PORT" "3305")
(setenv "SWIT_SUPPORT_MYSQL_USERNAME" "richard")
(setenv "SWIT_SUPPORT_MYSQL_PASSWORD" "kLcyf9IVJfw")
(setenv "SWIT_SUPPORT_MYSQL_DATABASE" "swit_support")
 
(setenv "SWIT_REDIS_ADDR" "10.6.0.3:6379")
(setenv "SWIT_REDIS_PASSWORD")

(defun jong-swit-dotenv1-load()
  (interactive)
  (load-file (format "%s/.emacs.d/jongyoungcha/jong-swit-dotenv1.el" (getenv "HOME")))
  )


(provide 'jong-swit-dotenv1)
