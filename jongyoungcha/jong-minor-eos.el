
(require 'subr-x)
(defvar name-nodeos)
(defvar name-keosd)
(defvar name-cleos)

(defvar jong-eos-buffer-name)
(defconst jong-eos-account-cache-path "~/.emacs.d/eos_account.cache")
(defconst jong-eos-wallet-cache-path "~/.emacs.d/eos_wallet.cache")
(defvar jong-eos-account-cache-buffer)
(defvar jong-eos-wallet-cache-buffer)
(defvar jong-eos-prev-buffer)
(defvar jone-eos-wallet-list)
(defvar jone-eos-account-list)

(defvar cleos-wallet-list-cmd)
(defvar cleos-wallet-create-cmd)
(defvar cleos-wallet-open-cmd)
(defvar cleos-wallet-lock-cmd)
(defvar cleos-wallet-unlock-cmd)
(defvar cleos-key-create-cmd)

(defvar wallet)
(defvar account)
(defstruct wallet
  name
  mkey)

(defstruct account
  wallet
  name
  pbkey
  prkey)

(setq name-nodeos "nodeos")
(setq name-keosd "keosd")
(setq name-cleos "cleos")
(setq jong-eos-buffer-name "*jong-eos-output*")
(progn (setq jong-eos-prev-buffer (current-buffer))
       (setq jong-eos-account-cache-buffer (find-file jong-eos-account-cache-path))
       (setq jong-eos-wallet-cache-buffer (find-file jong-eos-wallet-cache-path))
       (set-window-buffer (selected-window) jong-eos-prev-buffer))

(defun compile-eos-project
    (interactive)
  (message "called compile-eos-project"))


(defun executable-find (command)
  (interactive)
  "Search for COMMAND in `exec-path' and return the absolute file name.
Return nil if COMMAND is not found anywhere in `exec-path'."
  ;; Use 1 rather than file-executable-p to better match the behavior of
  ;; call-process.
  (locate-file command exec-path exec-suffixes 1))


(defun check-nodeos-existing()
  "If nodeos was existing return a bin name, Otherwise return nil"
  (if (executable-find name-nodeos)
      name-nodeos
    nil))


(defun check-keosd-existing()
  "If keosd was existing return a bin name, 
	Otherwise return nil"
  (if (executable-find name-keosd)
      name-keosd
    nil))


(defun check-cleos-existing()
  "If cleos was existing return a bin name, 
	Otherwise return nil"
  (if (executable-find name-cleos)
      name-cleos
    nil))


(setq cleos-wallet-list-cmd "%s wallet list")
(defun eos-wallet-list ()
  "If wallet was existing, Conduct a eos wallet list command."
  (interactive)
  (let (bin cmd)
    (setq bin (check-cleos-existing))
    (if bin
        (progn
          (setq cmd (format cleos-wallet-list-cmd bin))
          (exec-shell-command-with-buffer cmd jong-eos-buffer-name)
          (my-prev-window))
      (message "binary was not existing..."))
    ))


(setq cleos-wallet-create-cmd "%s wallet create -n %s")
(defun eos-wallet-create ()
  "If wallet was not existing, Conduct a eos wallet create command."
  (interactive)
  (let (bin cmd name)
    (setq bin (check-cleos-existing))
    (if bin
        (progn
          (setq name (read-string "(Create Wallet) Enter the wallet name : "))
          (setq cmd (format cleos-wallet-create-cmd bin name))
          (exec-shell-command-with-buffer cmd jong-eos-buffer-name)
          (my-prev-window))))
  )


(setq cleos-wallet-open-cmd "%s wallet open -n %s")
(defun eos-wallet-open ()
  "Open the wallet, If "
  (interactive)
  (let (bin cmd name)
    (setq bin (check-cleos-existing))
    (if bin
        (progn
          (setq name (read-string "(Open Wallet) Enter the wallet name : "))
          (when (equal "" name)
            (setq name "default"))
          (setq cmd (foramt cleos-wallet-open-cmd bin name))
          (exec-shell-command-with-buffer cmd jong-eos-buffer-name)
          (my-prev-window))))
  )


(setq cleos-wallet-lock-cmd "%s wallet lock -n %s")
(defun eos-wallet-lock ()
  "Lock the wallet,"
  (interactive)
  (let (bin cmd name)
    (setq bin (check-cleos-existing))
    (if bin
        (progn
          (setq name (read-string "(Lock Wallet) Enter the wallet name : "))
          (when (equal "" name)
            (setq name "default"))
          (setq cmd (format cleos-wallet-lock-cmd bin name))
          (exec-shell-command-with-buffer cmd jong-eos-buffer-name)
          (my-prev-window))))
  )


(setq cleos-wallet-unlock-cmd "%s wallet unlock -n %s")
(defun eos-wallet-unlock ()
  "Unlock the wallet"
  (interactive)
  (let (bin cmd name result)
    (setq bin (check-cleos-existing))
    (if bin
        (progn
          (setq name (read-string "(Unlock Wallet) Enther the wallet name : "))
          (when (equal "" name)
            (setq name "default"))
          (setq cmd (format cleos-wallet-unlock-cmd bin name))
          (exec-shell-command-with-buffer cmd jong-eos-buffer-name)
          (my-prev-window))))
  )


(setq cleos-key-create-cmd "%s create key")
(defun jong-eos-create-key ()
  (interactive)
  (let (bin cmd name output lines line pbkey prkey)
    (setq bin (check-cleos-existing))
    (if bin
        (progn
          (setq cmd (format cleos-key-create-cmd bin))
          (exec-shell-command-with-buffer cmd jong-eos-buffer-name)
          (setq output (buffer-string))

          ;; (message-box "%s" (buffer-name))
          ;; (meesage-box "%s" output)
          ;; ;; split lines
          (setq lines (split-string output "\n"))
          (setq prkey (car (split-string (car (nth 0 lines)) ":")))
          (setq pbkey (car (split-string (car (nth 1 lines)) ":")))

          ;; (message-box "%s" (nth 0 lines))
          ;; (message-box "%s" (nth 1 lines))
          ;; (message-box "%s" (nth 2 lines))
          ;; public k
          ;; return keys
          ;; (my-prev-window)
          )))
  )


(defun eos-wallet-unlock ()
  (interactive)
  (let (bin cmd name)
    (setq bin (check-cleos-existing))))


(defun jong-kill-eos-temp-buffer ()
  (interactive)
  (kill-buffer jong-eos-buffer-name))


(defun jong-eos-find-wallet-cache-buffer()
  (let ()
    (setq jong-eos-prev-buffer (current-buffer))
    (setq jong-eos-wallet-cache-buffer (find-file jong-eos-wallet-cache-path))
    jong-eos-wallet-cache-buffer)
  )

(defun jong-eos-close-wallet-cache-buffer()
  (let ()
    (setq jong-eos-wallet-cache-buffer (find-file jong-eos-wallet-cache-path))
    (when (not (equal jong-eos-wallet-cache-buffer nil))
      (kill-buffer jong-eos-wallet-cache-buffer)))
  )

(defun jong-eos-goto-prev-buffer()
  (when (buffer-live-p jong-eos-prev-buffer)
    (set-window-buffer (selected-window) jong-eos-prev-buffer))
  )

(defun jong-eos-find-public-key (wallet-name)
  (interactive)
  (let (key-buffer)
    (setq key-buffer (jong-eos-find-wallet-cache-buffer))
    ))

(defun jong-eos-get-wallet-list()
  (interactive)
  (let (key-buffer temp-string ret-value temp-list temp-sub-list record-list name mkey)
    (setq jong-eos-wallet-list '())
    (setq key-buffer (jong-eos-find-wallet-cache-buffer))
    (setq temp-string (buffer-string))
    (jong-eos-close-wallet-cache-buffer)
    (if (not (string= temp-string ""))
        (progn
          (setq temp-list (split-string temp-string "\n"))
          (dolist (elt-str temp-list ret-value)
            (when elt-str
              (progn
                (setq temp-sub-list (split-string elt-str " "))
                (setq name (nth 0 temp-sub-list))
                (setq mkey (nth 1 temp-sub-list))
                (setq temp-wallet (make-wallet :name namep
                                               :mkey mkey))
                (setq jong-eos-wallet-list (cons temp-wallet jong-eos-wallet-list)))))
          jong-eos-wallet-list)
      (progn
        (message "The contents of the wallet cache was empty... ( path : %s )" jong-eos-wallet-cache-path)
        nil)))
  )


(defun jong-eos-exists-wallet()
  )


(defun jong-eos-create-wallet ()
  (interactive)
  
  )

(defun jone-eos-delete-wallet()
  (interactive)
  )


(defun jong-eos-exists-account (name)
  (let (elem-list)
    (setq elem-list (jong-eos-get-account-list))
    (dolist (elt elem-list ret-value)
      (when (and elt (string= (account-name elt) name))
        (setq ret-value name)
        (return)))
    ret-value)
  )


(defun jong-eos-add-account ()
  (Let (name prkey pbkey)
       (setq name (read-string "Please input a name of account : "))
       (setq prkey (read-string "Please input a private key of account : "))
       (setq pbkey (read-string "Please input a public key of account : ")))
  )


(defun jong-eos-find-account-cache-buffer()
  (let()
    (setq jong-eos-prev-buffer (current-buffer))
    (setq jong-eos-account-cache-buffer (find-file jong-eos-account-cache-path))
    jong-eos-account-cache-buffer)
  )

(defun jong-eos-close-account-cache-buffer()
  (let ()
    (setq jong-eos-prev-buffer (current-buffer))
    (when (not (equal jong-eos-account-cache-buffer nil))
      (kill-buffer jong-eos-account-cache-buffer)))
  )

(defun jong-eos-get-account-list()
  (interactive)
  (let (cache-buffer temp-string ret-value temp-list temp-sub-list record-list wallet name pbkey prkey)
    (setq jong-eos-account-list '())
    (setq cache-buffer (jong-eos-find-account-cache-buffer))
    (setq temp-string (buffer-string))
    (jong-eos-close-account-cache-buffer)
    (if (not (string= temp-string ""))
        (progn
          (setq temp-list (split-string temp-string "\n"))
          (dolist (elt-str temp-list ret-value)
            (when elt-str
              (progn
                (setq temp-sub-list (split-string elt-str " "))
                (setq wallet (nth 0 temp-sub-list))
                (setq name (nth 1 temp-sub-list))
                (setq pbkey (nth 2 temp-sub-list))
                (setq prkey (nth 3 temp-sub-list))
                (setq temp-account (make-account :wallet wallet
                                                 :name name
                                                 :pbkey pbkey
                                                 :prkey prkey))
                (setq jong-eos-account-list (cons temp-account jong-eos-account-list)))))
          jong-eos-account-list)
      (progn
        (message "The contents of the account cache was empty... ( path : %s )" jong-eos-account-cache-path)
        nil)))
  )

(defun jong-eos-select-account-public-key ()
  (interactive)
  (let (elem-list temp-str str-list)
    (setq elem-list (jong-eos-get-account-list))
    (dolist (elem elem-list str-list)
      (when elem
        (progn
          (setq temp-str (format "[%s] %s : %s" (account-wallet elem) (account-name elem) (account-pbkey elem)))
          (setq str-list (cons temp-str str-list)))))
    
    (jong-eos-goto-prev-buffer)
    (helm :sources (helm-build-sync-source "EOS public key list"
                     :candidates str-list
                     :fuzzy-match t
                     :action (lambda (candidate)
                               candidate))
          :buffer "*jong-eos-public-key-list"))
  )


(defun jong-eos-select-account-private-key ()
  (interactive)
  (let (elem-list temp-str str-list)
    (setq elem-list (jong-eos-get-account-list))
    (dolist (elem elem-list str-list)
      (when elem
        (progn
          (setq temp-str (format "[%s] %s : %s" (account-wallet elem) (account-name elem) (account-prkey elem)))
          (setq str-list (cons temp-str str-list)))))
    (jong-eos-goto-prev-buffer)
    (helm :sources (helm-build-sync-source "EOS private key list"
                     :candidates str-list
                     :fuzzy-match t
                     :action (lambda (candidates)
                               (setq candidates (helm-marked-candidates))
                               candidates))
          :buffer "*jong-eos-public-key-list"))
  )


(defun jong-eos-delete-account ()
  (interactive)
  (let (all-elem-list selected-elem-list tmep-str str-list str-all-list)
    (setq selected-elem-list (jong-eos-select-account-private-key))
    (message "selected : %s" selected-elem-list)
    (setq all-elem-list (jong-eos-get-account-list))
    (dolist (elem-selected selected-elem-list str-list)
      (when elem-selected
        (message "%s" elem-selected)
        (elem-selected)
        (dolist (elem all-elem-list str-all-list)
          (when elem)
          )
        )
      )
    )
  )

(defun jong-eos-create-account ()
  (interactive)
  (setq test (jong-eos-select-account-private-key))
  )

(define-minor-mode jong-eos-mode
  :lighter " jong-eos"
  :keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c e w l") 'eos-wallet-list)
    (define-key map (kbd "C-c e w c") 'eos-wallet-create)
    (define-key map (kbd "C-c e w o") 'eos-wallet-open)
    (define-key map (kbd "C-c e w k") 'eos-wallet-lock)
    (define-key map (kbd "C-c e w u") 'eos-wallet-unlock)
    (define-key map (kbd "C-c e k") 'jong-kill-eos-temp-buffer)
    map))

(provide 'jong-minor-eos)


