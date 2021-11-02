;;; Code:

(defconst jong-java-output-buffer-name "*jong-java-output*"
  "A Ouput Buffur of java.")

(defconst jong-java-output-buffer-name "*jong-java-debug*"
  "A Debug Buffur of java.")

(defcustom jong-java-process-names-to-kill '("PlatformServerApplication")
  "Target Processes to kill in java mode."
  :group 'jong-java
  :type 'list)



(add-to-list #'jong-kill-buffer-patterns jong-java-output-buffer-name)
(add-to-list #'jong-kill-buffer-patterns "*HTTP Response*")

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


(defun jong-java-install-eclim-server ()
  (interactive)
  (let ((cmd))
	(setq cmd (concat
			   "cd;"
			   (format "wget http://mirrors.neusoft.edu.cn/eclipse/technology/epp/downloads/release/2019-03/R/eclipse-jee-2019-03-R-linux-gtk-x86_64.tar.gz")
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


(defun jong-java-kill-target-processes ()
  (interactive)
  (let ((cmd)
		(pid)
		(pname))
	(with-current-buffer (get-buffer-create jong-java-output-buffer-name)
	  (dolist (pname jong-java-process-names-to-kill)
		(display-buffer (current-buffer))
		(setq cmd (format "ps -ef | grep %s | awk '{print $2}' | xargs -i kill -9 {}" pname))
		(async-shell-command cmd (current-buffer) (current-buffer)))
	  )
	)
  )

(defun jong-java-debug-mode-screen ()
  (interactive)
  (jong-common-set-window-1-2 (buffer-name (current-buffer)) "*out*" "*dap-ui-repl*"))

(use-package lsp-java
  :ensure t
  :config
  (add-hook 'java-mode-hook 'lsp)
  (setq lsp-java-inhibit-message nil)
  (setq lsp-java-java-path (format "%s/bin/java" (getenv "JAVA_HOME")))
  (setq lsp-java-save-actions-organize-imports t)
  (setq lsp-java-content-provider-preferred "fernflower"))
  ;; (setq lsp-java-vmargs
  ;; (list
  ;; "-noverify"
  ;; "-Xmx1G"
  ;; "-XX:+UseG1GC"
  ;; "-XX:+UseStringDeduplication"
  ;; (format "-javaagent:/%s/%s" (getenv "HOME") ".m2/repository/org/projectlombok/lombok/1.18.2/lombok-1.18.2.jar")))
;; (use-package dap-java
;; :ensure t)

(add-hook 'java-mode-hook (lambda()
							;; (local-set-key (kbd "C-c C-c") 'lsp-execute-code-action)
							(local-set-key (kbd "C-c k") 'lsp-ui-doc-show)
							(local-set-key (kbd "C-c r w") 'lsp-workspace-restart)

							(local-set-key (kbd "M-.") 'lsp-ui-peek-find-definitions)
							(local-set-key (kbd "C-c r ,") 'lsp-ui-peek-find-references)
							(local-set-key (kbd "C-c r i") 'lsp-ui-peek-find-implementation)
							(local-set-key (kbd "C-c o i") 'lsp-ui-organize-imports)
							(local-set-key (kbd "C-c r l") 'helm-imenu)
							(local-set-key (kbd "C-c r r") 'lsp-rename)
							))


(defun jong-java-setting-environment()
  "Setting environment and keybindings."
  (clang-format-buffer)
  )

(provide 'jong-java)
