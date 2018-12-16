;;;

(require 'cl)

(defcustom ether-target-eshell
  :type 'string)
(setq ether-target-eshell "*chan-dlv-server*")

(cl-defstruct ether-node
  name
  host
  user
  passwd
  privkey-path
  testnet-dir)

(defcustom main-node
  :type 'ether-node)
(setq main-node (make-ether-node
                 :name "ethermain"
                 :host "192.168.130.100"
                 :user "jongyoungcha"
                 :passwd "jongyoungcha"
                 :testnet-dir "~/testnet"))


(defcustom ether-node-list
  :type 'list)
(setq ether-node-list (list
                       ;; (make-ether-node
                       ;;  :name "ethernode1"
                       ;;  :host "192.168.130.101"
                       ;;  :user "jongyoungcha"
                       ;;  :passwd "jongyoungcha"
                       ;;  :privkey-path "~/node_privs/node1_rsa"
                       ;;  :testnet-dir "~/testnet")
                       ;; (make-ether-node
                       ;; :name "ethernode2"
                       ;; :host "192.168.130.102"
                       ;; :user "jongyoungcha"
                       ;; :passwd "jongyoungcha"
                       ;; :privkey-path "~/node_privs/node2_rsa"
                       ;; :testnet-dir "~/testnet"))
                       (make-ether-node
                        :name "node_inner"
                        :host "192.168.56.102"
                        :user "jongyoungcha"
                        :passwd "jongyoungcha"
                        :privkey-path "~/node_privs/nodeinner_rsa"
                        :testnet-dir "~/testnet")))


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
      (eshell-command (format "ssh node_inner"))
      )))

(defun chan-get-ether-peer-informaition (node)
  (interactive)
  ()
  )



(defun chan-add-peers ()
  (interactive)
  (let ((target-buffer "*chan-dlv-server*"))
    (with-current-buffer (get-buffer-create target-buffer)
      (dolist (elem-node ether-node-list)
        (chan-eshell-exec-cmd (current-buffer)
                              (format "admin.addPeer(\"%s\")" ))
        ))))


(defun chan-init-ether-nodes ()
  (interactive)
  (let ((target-buffer "*EShell Command Output*"))
    (dolist (elem-node ether-node-list)
      (with-current-buffer (get-buffer-create target-buffer)
        (eshell-mode)
        (display-buffer target-buffer)
        (chan-eshell-exec-cmd (current-buffer)
                              "ssh node_inner")
        (chan-eshell-exec-cmd (current-buffer)
                              (format "rm -rf %s" (ether-node-testnet-dir elem-node)))
        (chan-eshell-exec-cmd (current-buffer)
                              (format "geth --datadir=%s init %s/genesis.json"
                                      (ether-node-testnet-dir elem-node)
                                      (ether-node-testnet-dir elem-node)))
        (chan-eshell-exec-cmd (current-buffer)
                              (format "geth --datadir=%s console" (ether-node-testnet-dir elem-node)))
        ))))


(defun ether-test-test ()
  (interactive)
  (connect-node main-node)
  )


(defun chan-init-directory ()
  (interactive)
  "Chan connect node."
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
        ;; (eshell-command (format "echo test!!!"))
        )
    )
  )

(provide 'jong-ether-test)
