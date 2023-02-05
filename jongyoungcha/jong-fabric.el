(require 'ivy)

(defun jong-fabric-decribe-certificate(&optional path)
  ""
  (interactive)
  (let (certificate)
    (when (= (length path) 0)
      (setq certificate (my-counsel-find-directory))
      )
    (shell-command (format "openssl x509 -text -noout -in %s" certificate))
    ))

(defun my-counsel-find-directory (&optional start-dir)
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
