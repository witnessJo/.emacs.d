;;;

(require 'cl)

(defcustom ether-target-eshell
  :type 'string)
(setq ether-target-eshell "*chan-dlv-server*")

(cl-defstruct ether-node
  name
  host
  enode
  user
  passwd
  privkey-path
  testnet-dir)

(defcustom main-node
  :type 'ether-node)
(setq main-node (make-ether-node
                 :name "ethermain"
                 :host "192.168.130.100"
                 :enode "enode://0fdc5ec82ef963a19cde7861554f0ddb06e5f1021de35b89bff8824f20e6a7094f5ab8c37d0c7682f18e76d0641ec593741b7f8cbff8cf9f5e9c31e10da69009@192.168.130.100:30303?discport=0"
                 :user "jongyoungcha"
                 :passwd "jongyoungcha"
                 :testnet-dir "~/testnet"))


(defcustom ether-node-list
  :type 'list)
(setq ether-node-list (list
                       ;; (make-ether-node
                       ;; :name "ethernode1"
                       ;; :host "192.168.130.101"
                       ;; :user "jongyoungcha"
                       ;; :enode ""
                       ;; :passwd "jongyoungcha"
                       ;; :privkey-path "~/ethernode_keys/ethernode1_rsa"
                       ;; :testnet-dir "~/testnet")
                       ;; (make-ether-node
                       ;; :name "ethernode2"
                       ;; :host "192.168.130.102"
                       ;; :enode ""
                       ;; :user "jongyoungcha"
                       ;; :passwd "jongyoungcha"
                       ;; :privkey-path "~/ethernode_keys/ethernode2_rsa"
                       ;; :testnet-dir "~/testnet")
                       (make-ether-node
                        :name "ethernodeinner"
                        :host "192.168.56.102"
                        :user "jongyoungcha"
                        :passwd "jongyoungcha"
                        :privkey-path "~/ethernode_keys/ethernodeinner_rsa"
                        :testnet-dir "~/testnet")
                       ))


(defun connect-node-tramp (node)
  (interactive)
  (let ((default-directory)
        (node-buffer-name))
    (setq node-buffer-name (format "*eshell-%s*" (ether-node-name node)))
    (with-current-buffer (get-buffer-create node-buffer-name)
      (setq default-directory (format "/ssh:%s@%s:%s"
                                      (ether-node-user node)
                                      (ether-node-host node)
                                      (ether-node-testnet-dir node)))
      (display-buffer node-buffer-name)
      (eshell-mode)
      )))

(defun chan-eshell-exec-cmd (target-buffer cmd)
  (condition-case ex
      (with-current-buffer target-buffer
        (ignore-errors (eshell-return-to-prompt))
        (autopair-newline)
        (goto-char (point-max))
        (insert cmd)
        (autopair-newline)
        (ignore-errors (eshell-return-to-prompt))
        )))


(defun chan-connect-node-ssh (node)
  (let ((target-buffer))
    (setq target-buffer "*EShell Command output*")
    (with-current-buffer (get-buffer-create target-buffer)
      (display-buffer target-buffer)
      (eshell-command "ls")
      (eshell-command (format "ssh ethernode_inner"))
      )))

;; (defun chan-get-ether-enode-information (node)
;;   "Connect to ethererum node and when get the enode information return the value."
;;   (interactive)
;;   ;; Check if there is geth binary file.
;;   ;; Connect to the serve with eshell.
;;   ;; Get node information with geth command.
;;   ;; Parse stdout of geth command and return node information.
;;   )

(defun chan-add-peer-ethernode (target-buffer)
  (interactive)
  (let ((cmd-addpeer))
    (with-current-buffer target-buffer
      (autopair-newline)
      (goto-char (point-max))
      (setq cmd-addpeer (format "admin.addPeer(\"%s\")" (ether-node-enode main-node)))
      (insert cmd-addpeer)
      (autopair-newline)
      ))
  )


(defun chan-init-ethernodes ()
  (interactive)
  (let ((target-buffer)
        (base-host)
        (genesis-json-path)
        (testnet-dir))
    (setq genesis-json-path "~/testnet/genesis.json")
    (dolist (elem-node ether-node-list)
      (setq base-host (format "/ssh:%s" (ether-node-name elem-node)))
      (setq testnet-dir (ether-node-testnet-dir elem-node))
      (setq target-buffer (format "*%s*" (ether-node-name elem-node)))
      (with-current-buffer (get-buffer-create target-buffer)
        (setq default-directory (format "%s:~" base-host))
        (start-file-process "rm" (get-buffer-create target-buffer)
                            "/bin/bash" "-c" (format "rm -rf %s" testnet-dir))
        (start-file-process "mkdir" (get-buffer-create target-buffer)
                            "/bin/bash" "-c" (format "mkdir %s" testnet-dir))
        (copy-file genesis-json-path (format "%s:~/testnet/genesis.json" base-host))
        (start-file-process "~/goworks/bin/geth"
                            (get-buffer-create target-buffer)
                            "/bin/bash" "-c"
                            (format "~/goworks/bin/geth --datadir=%s init %s/genesis.json"
                                    testnet-dir testnet-dir))
        (start-file-process "~/goworks/bin/geth"
                            (get-buffer-create target-buffer)
                            "/bin/bash" "-c"
                            (format "~/goworks/bin/geth --datadir=%s console" testnet-dir))
        (ignore-errors (call-interactively 'term-mode))
        
        (goto-char (point-max))
        (insert "personal.newAccount(\"jongyoungcha\")")
        (autopair-newline)
        (insert "personal.newAccount(\"jongyoungcha\")")
        (autopair-newline)
        (insert "personal.unlockAccount(eth.accounts[0], \"jongyoungcha\", 0)")
        (autopair-newline)
        (insert "personal.unlockAccount(eth.accounts[1], \"jongyoungcha\", 0)")
        
        (chan-add-peer-ethernode (current-buffer))
        
        (display-buffer target-buffer)
        )))
  )


(defun chan-init-ethernode (target-buffer)
  "'target-buffer' must be a 'eshell-mode buffer."
  (with-current-buffer target-buffer
    ;; (chan-eshell-exec-cmd (current-buffer)
    ;; (format "rm -rf %s" (ether-node-testnet-dir elem-node)))
    (chan-eshell-exec-cmd (current-buffer)
                          (format "geth --datadir=%s init %s/genesis.json"
                                  (ether-node-testnet-dir elem-node)
                                  (ether-node-testnet-dir elem-node)))
    (chan-eshell-exec-cmd (current-buffer)
                          (format "geth --datadir=%s console" (ether-node-testnet-dir elem-node))))
  )


(defun ether-test-test ()
  (interactive)
  (connect-node main-node)
  )



(defun chan-check-dlv-server-buffer ()
  "Check is there target buffer..."
  (let ((target-buffer))
    (if (setq target-buffer (get-buffer ether-target-eshell))
        target-buffer
      nil))
  )

(defun chan-ether-send-transaction ()
  "Send transaction coinbase to accounts[1]."
  (interactive)
  (condition-case ex
      (with-current-buffer ether-target-eshell
        (goto-char (point-max))
        (insert (format "eth.sendTransaction({from:eth.coinbase, to:eth.accounts[1], value:1})"))
        (eshell-send-input)
        (goto-char (point-max))
        (eshell-return-to-prompt))
    (message ex))
  )

(defun chan-ether-new-account ()
  "This is unlock coinbase."
  (interactive)
  (condition-case ex
      (with-current-buffer ether-target-eshell
        (goto-char (point-max))
        (insert (format "personal.newAccount(\"jongyoungcha\")"))
        (eshell-send-input)
        (goto-char (point-max))
        (eshell-return-to-prompt))
    (message ex))
  )


(defun chan-ether-unlock-account0 ()
  "This is unlock coinbase."
  (interactive)
  (condition-case ex
      (with-current-buffer ether-target-eshell
        (goto-char (point-max))
        (insert (format "personal.unlockAccount(eth.accounts[0], \"jongyoungcha\", 0)"))
        (eshell-send-input)
        (goto-char (point-max))
        (eshell-return-to-prompt))
    (message ex))
  )


(defun chan-ether-unlock-account1 ()
  "This is unlock coinbase."
  (interactive)
  (condition-case ex
      (with-current-buffer ether-target-eshell
        (goto-char (point-max))
        (insert (format "personal.unlockAccount(eth.accounts[1], \"jongyoungcha\", 0)"))
        (eshell-send-input)
        (goto-char (point-max))
        (eshell-return-to-prompt))
    (message ex))
  )


(defun chan-ether-get-peers ()
  "This is unlock coinbase."
  (interactive)
  (condition-case ex
      (with-current-buffer ether-target-eshell
        (goto-char (point-max))
        (insert (format "admin.peers"))
        (eshell-send-input)
        (goto-char (point-max))
        (eshell-return-to-prompt))
    (message ex))
  )


(defun chan-ether-init-testnet ()
  "Initialize the testnet."
  (interactive)
  (condition-case ex
      ;; (with-current-buffer (eshell-return-to-prompt)
      (with-current-buffer (get-buffer "*eshell*")
        ;; (insert (format "admin.peers"))
        (goto-char (point-max))
        (Insert "ls")
        (eshell-send-input)
        (eshell-return-to-prompt)
        (goto-char (point-max))
        (eshell-return-to-prompt)
        )
    )
  )

(provide 'jong-ether-test)

