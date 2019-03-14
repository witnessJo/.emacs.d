;;; Code


;; (add-to-list 'tramp-connection-properties
;; (list (regexp-quote "/ssh:user@randomhost.you.domain:")
;; "remote-shell" "/bin/bash"))

(defcustom jong-brth-user "root"
  "ID for connection."
  :type 'string)

(defcustom jong-brth-passwd "dusrn"
  "Passwd for connection."
  :type 'string)

(defcustom jong-brth-node-list (list "192.168.55.100"
				     "192.168.55.101"
				     "192.168.55.102"
				     "192.168.55.103"
				     "192.168.0.160"
				     "192.168.0.161"
				     "192.168.0.162"
				     "192.168.0.163")
  "Berith node list."
  :type 'list)


(defcustom jong-brth-node-commands (list "eth\n"
					 "miner.start(1)\n"
					 "miner.stop()\n"
					 "miner.setEtherbase(eth.accounts[0])\n"
					 "personal.newAccount(\"berith\")\n"
					 "personal.unlockAccount(eth.accounts[0], \"berith\", 0)\n"
					 "personal.lockAccount(eth.accounts[0], \"berith\", 0)\n"
					 "web3.fromWei(berith.getStakeBalance(eth.accounts[0]), \"ether\")\n"
					 "web3.fromWei(eth.getBalance(eth.coinbase), \"ether\")\n"
					 "web3.fromWei(berith.getRewardBalance(eth.coinbase), \"ether\")\n"
					 "berith.stake({from:eth.coinbase, value:1000000000000000000, staking:true})\n"
					 "berith.stopStaking(eth.coinbase)\n"
					 "eth.sendTransaction({from:eth.accounts[0], to:\"0x496732e14c615792dd9dc09404387938e3a8a407\", value:200000000000000000000})"
					 "admin.addPeer(\"\")")

  "Berith command list."
  :type 'list)



(defun jong-brth-print-node-list()
  (interactive)
  (message "%s" jong-brth-node-list))


(setq jong-brth-test-datadir (format "%s/%s" (getenv "HOME") "testdir"))
(setq jong-brth-highlights
      '(("test\\|success\\|done\\|INFO\\|" . font-lock-function-name-face)
	("failed\\|Failed\\|FAILED\\|warn\\|Warn\\|WARN\\|" . font-lock-warning-face)))


(define-derived-mode jong-brth-attach-mode eshell-mode "BRTH-ATTCH"
  (setq font-lock-defaults '(jong-brth-highlights)))


(defun jong-brth-exec-command (cmd)
  "just execution of berith command."
  (goto-char (point-max))
  (insert (format "%s" cmd))
  )

(defun jong-brth-get-log-buffer-name (node-host)
  (format "*brth-node-log-%s*" node-host))

(defun jong-brth-show-node-log (node-host)
  "A."
  (let ((node-host-buffer-name (jong-brth-get-log-buffer-name node-host))
	(node-host-buffer))
    (if (setq node-host-buffer (get-buffer node-host-buffer-name))
	(if (get-buffer-process node-host-buffer)
	    (progn
	      (switch-to-buffer node-host-buffer)
	      (goto-char (point-min))
	      (goto-char (point-max)))
	  (with-current-buffer (get-buffer-create node-host-buffer-name)
	    (setq default-directory (format "/ssh:%s@%s:" jong-brth-user node-host))
	    (ignore-errors (async-shell-command "tail -f ./geth.log" (current-buffer) (current-buffer)))
	    (other-window 1)
	    (sleep-for 1)
	    (goto-char (point-min))
	    (goto-char (point-max))
	    ))
      (with-current-buffer (get-buffer-create node-host-buffer-name)
	(setq default-directory (format "/ssh:%s@%s:" jong-brth-user node-host))
	(ignore-errors (async-shell-command "tail -f ./geth.log" (current-buffer) (current-buffer)))
	(other-window 1)
	(sleep-for 1)
	(goto-char (point-min))
	(goto-char (point-max)))
      ))
  )

(defun jong-brth-select-node-log ()
  (interactive)
  (let ((selected-node nil))
    (setq selected-node  (helm :sources (helm-build-sync-source "Berith nodes for log."
					  :candidates jong-brth-node-list
					  :fuzzy-match t
					  :action (lambda (node)
						    node))
			       :buffer "*jong-berith-nodes-log*"))
    (when selected-node (jong-brth-show-node-log selected-node)))
  )

(defun jong-brth-get-attach-buffer-name (node-host)
  (format "*brth-node-attach-%s*" node-host))


(defun jong-brth-show-node-attach (node-host)
  "A."
  (let ((node-host-buffer-name (jong-brth-get-attach-buffer-name node-host))
	(node-host-buffer))
    (if (setq node-host-buffer (get-buffer node-host-buffer-name))
	(if (get-buffer-process node-host-buffer)
	    (progn
	      (switch-to-buffer node-host-buffer)
	      (goto-char (point-min))
	      (goto-char (point-max)))
	  (with-current-buffer (get-buffer-create node-host-buffer-name)
	    (setq default-directory (format "/ssh:%s@%s:" jong-brth-user node-host))
	    (ignore-errors (shell (current-buffer)))
	    (jong-brth-exec-command "source ./.bash_profile\n")
	    (jong-brth-exec-command "brth-attach\n")
	    (goto-char (point-min))
	    (goto-char (point-max))
	    ))
      (with-current-buffer (get-buffer-create node-host-buffer-name)
	(setq default-directory (format "/ssh:%s@%s:" jong-brth-user node-host))
	(ignore-errors (shell (current-buffer)))
	(jong-brth-exec-command "source ./.bash_profile\n")
	(jong-brth-exec-command "brth-attach\n")
	(goto-char (point-min))
	(goto-char (point-max))
	)
      ))
  )

(defun jong-brth-send-command ()
  "Send input message."
  (interactive)
  (let ((brth-command nil))
    (setq brth-command  (helm :sources (helm-build-sync-source "Berith Commands."
					 :candidates jong-brth-node-commands
					 :fuzzy-match t
					 :action (lambda (node)
						   node))
			      :buffer "*Berith Commands*"))
    
    (jong-brth-exec-command brth-command))
  )



(defun jong-brth-select-node-attach ()
  (interactive)
  (let ((selected-node nil))
    (setq selected-node (helm :sources (helm-build-sync-source "Berith nodes for attach."
					 :candidates jong-brth-node-list
					 :fuzzy-match t
					 :action (lambda (node)
						   node))
			      :buffer "*jong-berith-nodes-attach*"))
    (when selected-node (jong-brth-show-node-attach selected-node))
    )
  )


(define-derived-mode jong-brth-log-mode eshell-mode "BRTH-LOG"
  (setq font-lock-defaults '(jong-brth-highlights)))


(require 'shell)
(global-set-key (kbd "C-c r b l") 'jong-brth-select-node-log)
(global-set-key (kbd "C-c r b a") 'jong-brth-select-node-attach)
(define-key shell-mode-map (kbd "C-c c") 'jong-brth-send-command)

(provide 'jong-brth-test)
