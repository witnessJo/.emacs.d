;;; Code

(require 'ivy)

(setenv "FABRIC_SAMPLE_HOME" (concat (getenv "HOME") "/work/fabric-samples"))
(setenv "FABRIC_TEST_NETWORK_HOME" (concat (getenv "HOME") "/work/fabric-samples/test-network"))
(setenv "FABRIC_TEST_NETWORK_NAME" "richard-network")
(setenv "FABRIC_CFG_PATH" (concat (getenv "FABRIC_SAMPLE_HOME") "/config/"))


(defun jong-fabric-samples-restart()
  (interactive)
  (let ((default-directory (getenv "FABRIC_TEST_NETWORK_HOME")))
    (shell-command "./network.sh down")
    (shell-command (format "./network.sh createChannel -ca -c %s" (getenv "FABRIC_TEST_NETWORK_NAME")))
    ))

(defun jong-fabric-samples-set-org1()
  (interactive)
  (setenv "CORE_PEER_TLS_ENABLED" "true")
  (setenv "CORE_PEER_LOCALMSPID" "Org1MSP")
  (setenv "CORE_PEER_TLS_ROOTCERT_FILE"
          (concat (getenv "FABRIC_TEST_NETWORK_HOME")
                  "/organizations/peerOrganizations/org1.example.com/peers/peer0.org1.example.com/tls/ca.crt"))
  (setenv "CORE_PEER_MSPCONFIGPATH"
          (concat (getenv "FABRIC_TEST_NETWORK_HOME")
                  "/organizations/peerOrganizations/org1.example.com/users/Admin@org1.example.com/msp"))
  (setenv "CORE_PEER_ADDRESS" "localhost:7051")
  )

(defun jong-fabric-samples-set-org2()
  (interactive)
  (setenv "CORE_PEER_TLS_ENABLED" "true")
  (setenv "CORE_PEER_LOCALMSPID" "Org2MSP")
  (setenv "CORE_PEER_TLS_ROOTCERT_FILE"
          (concat (getenv "FABRIC_TEST_NETWORK_HOME")
                  "/organizations/peerOrganizations/org2.example.com/peers/peer0.org2.example.com/tls/ca.crt"))
  (setenv "CORE_PEER_MSPCONFIGPATH"
          (concat (getenv "FABRIC_TEST_NETWORK_HOME")
                  "/organizations/peerOrganizations/org2.example.com/users/Admin@org2.example.com/msp"))
  (setenv "CORE_PEER_ADDRESS" "localhost:9051")
  )

(defun jong-fabric-package-chaincode()
  (interactive)
  (shell-command
   (format
    "peer lifecycle chaincode package basic.tar.gz --path %s/../asset-transfer-basic/chaincode-go/ --lang golang --label basic_1.0"
    (getenv "FABRIC_TEST_NETWORK_HOME")
    ))
  )

(defun jong-fabric-install-package()
  (interactive)
  (let (file-path)
    (setq file-path (jong-counsel-find-directory))
    (print file-path)
    (shell-command (format "peer lifecycle chaincode install %s" file-path))
    )
  )

(defun jong-fabric-decribe-certificate(&optional path)
  ""
  (interactive)
  (let (certificate)
    (when (= (length path) 0)
      (setq certificate (my-counsel-find-directory))
      )
    (shell-command (format "openssl x509 -text -noout -in %s" certificate))
    ))

(defun jong-counsel-find-directory (&optional start-dir)
  "Return a directory chosen by the user.
The user is prompted to choose a directory starting with START-DIR."
  (let ((ivy-read-prompt "Choose directory: ")
        ;; (counsel--find-file-predicate #'file-directory-p)
        (default-directory (or start-dir default-directory)))
    (ivy-read
     ivy-read-prompt
     #'read-file-name-internal
     :matcher #'counsel--find-file-matcher
     )
    )
  )


(provide 'jong-fabric)
