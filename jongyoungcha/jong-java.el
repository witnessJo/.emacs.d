;;; Code:

(defconst jong-java-output-buffer-name "*jong-java-output*"
  "A Ouput Buffur of java.")

(defconst jong-java-output-buffer-name "*jong-java-debug*"
  "A Debug Buffur of java.")

(add-to-list #'jong-kill-buffer-patterns "\\*out\\*[\\<2\\>]*")

(defun jong-java-install-maven ()
  (interactive)
  (let ((maven-uri "http://mirror.navercorp.com/apache/maven/maven-3/3.6.0/binaries/apache-maven-3.6.0-bin.tar.gz")
		  (target-file "apache-maven-3.6.0-bin.tar.gz")
		  (extracted-dir "apache-maven-3.6.0")
		  (cmd))
	 (setq cmd (concat
			      "cd;"
			      (format "wget %s;" maven-uri)
			      (format "tar -xvf %s;" target-file)
			      (format "cd %s" extracted-dir)))
	 (with-current-buffer (get-buffer-create jong-java-output-buffer-name)
	   (display-buffer (current-buffer))
	   (async-shell-command cmd (current-buffer) (current-buffer))
	   )
	 )
  )


(defun jong-java-install-jdee-server ()
  (interactive)
  (let ((jdee-source-uri "https://github.com/jdee-emacs/jdee-server.git")
		  (clone-dir "jdee-server")
		  (cmd))
	 (setq cmd (concat
			      "cd;"
			      (format "git clone %s;" jdee-source-uri)
			      (format "cd %s;" clone-dir)
               "mvn -Dmaven.test.skip=true package"))
	 (with-current-buffer (get-buffer-create jong-java-output-buffer-name)
	   (display-buffer (current-buffer))
	   (async-shell-command cmd (current-buffer) (current-buffer))
	   )
	 )
  )


(defun jong-java-install-eclim-server ()
  (interactive)
  (let ((cmd))
    (setq cmd (concat
               "cd;"
               (format "wget https://mirrors.tuna.tsinghua.edu.cn/eclipse/technology/epp/downloads/release/2019-03/R/eclipse-jee-2019-03-R-linux-gtk-x86_64.tar.gz;")
               "tar -xvf eclipse-jee-2019-03-R-linux-gtk-x86_64.tar.gz;"
               (format "wget https://github.com/ervandew/eclim/releases/download/2.8.0/eclim_2.8.0.bin;")
               "chmod 744 ./eclim_2.8.0.bin;"
               "./eclim_2.8.0.bin;"
               ))
    (with-current-buffer (get-buffer-create jong-java-output-buffer-name)
	   (display-buffer (current-buffer))
	   (async-shell-command cmd (current-buffer) (current-buffer))
	   )
    )
  )


(use-package eclim
  :ensure t
  :config
  (setq eclimd-autostart t)
  (custom-set-variables
   '(eclim-eclipse-dirs '((format "%s/eclipse" (getenv "HOME"))))
   '(eclim-executable (format "%s/eclipse/eclim" (getenv "HOME")))
   )
  )


(use-package projectile :ensure t)
(use-package treemacs :ensure t)
(use-package yasnippet :ensure t)
(use-package lsp-mode :ensure t)
(use-package hydra :ensure t)
(use-package company-lsp :ensure t)
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :diminish lsp-ui
  :init (lsp-ui-mode)
  :config
  (eldoc-mode nil)
  (global-eldoc-mode -1)
  (setq lsp-ui-peek-enable nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-flycheck-enable t))

(use-package lsp-java :ensure t :after lsp
  :config (add-hook 'java-mode-hook 'lsp))

(use-package dap-mode
  :ensure t :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package dap-java :after (lsp-java))
(use-package lsp-java-treemacs :after (treemacs))

(define-key java-mode-map (kbd "C-c c r") 'eclim-run-class)
(define-key java-mode-map (kbd "C-c c p") 'eclim-project-create)
(define-key java-mode-map (kbd "C-c r r") 'lsp-rename)
(define-key java-mode-map (kbd "C-c r .") 'lsp-find-definition)
(define-key java-mode-map (kbd "C-c r ,") 'lsp-find-references)
(define-key java-mode-map (kbd "C-c d d") 'lsp-ui-doc-show)
(define-key java-mode-map (kbd "C-g") (lambda()
										          (interactive)
										          (jong-kill-temporary-buffers)
										          (lsp-ui-doc-hide)))

(define-key java-mode-map (kbd "<f5>") 'dap-java-debug)
(define-key java-mode-map (kbd "<f8>") 'dap-continue)
(define-key java-mode-map (kbd "<f9>") 'dap-breakpoint-toggle)
(define-key java-mode-map (kbd "<f10>") 'dap-next)
(define-key java-mode-map (kbd "<f11>") 'dap-step-in)
(define-key java-mode-map (kbd "<f12>") 'dap-step-in)

(add-hook 'java-mode-hook (lambda ()
                            ;; (eclim-mode t)
						          ))

(provide 'jong-java)
