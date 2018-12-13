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
  testnet-dir)

(defcustom ether-node-list
  :type 'list)
(setq ether-node-list (list
		       (make-ether-node
			:name "ethernode1"
			:host "192.168.130.101"
			:user "jongyoungcha"
			:passwd "jongyoungcha"
			:testnet-dir "~/testnet")
		       (make-ether-node
			:name "ethernode2"
			:host "192.168.130.102"
			:user "jongyoungcha"
			:passwd "jongyoungcha"
			:testnet-dir "~/testnet")))


(defun chan-init-directory ()
  (interactive)
  "Chan make"
  ;;; TODO ------ :)
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
        (insert "ls")
        (eshell-send-input)
        (eshell-return-to-prompt)
        (goto-char (point-max))
        (eshell-return-to-prompt)
        ;; (eshell-command (format "echo test!!!"))
        )
    )
  )

(provide 'jong-ether-test)
