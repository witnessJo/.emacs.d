;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; c develope environments ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar jong-c-output-buffer "*jong-c-output*" "Jong c language output buffer.")
(add-to-list #'jong-kill-buffer-patterns jong-c-output-buffer)


(defcustom jong-c-bin-name nil
  "Jong c language run command."
  :type 'string)


(defcustom jong-c-gud-buffer-name nil
  "Jong c gud-buffer name."
  :type 'string)


(defcustom jong-c-gud-args nil
  "Jong c debug-mode arguments."
  :type 'string)


(define-derived-mode jong-c-gud-mode  gud-mode "jong-gud-mode"
  (setq font-lock-defaults '(modern-c++-font-lock-keywords)))


(defun jong-c-gud ()
  (interactive)
  (call-interactively 'gud-gdb)
  (jong-c-gud-mode))


(defun jong-c-gud-find-buffer ()
  (interactive)
  (let ((jong-c-output-buffer))
    )
  )

(defun jong-c-gud-set-args ()
  (interactive)
  (let ((target-buffer nil)
		  (cmd nil))
	 ;; Set gud-gdb arguments.
	 (setq jong-c-gud-args (read-string "jong-gud-mode args : "))
    (setq target-buffer (get-buffer jong-c-gud-buffer-name))

	 ;; When buffer-name is existing, Get target buffer to send.
	 ;; (when (string= "" jong-c-gud-buffer-name)
	 ;; (setq target-buffer (catch 'found
	 ;; (dolist (buffer (buffer-list))
	 ;; (when (string= jong-c-gud-buffer-name (buffer-name buffer))
	 ;; (throw 'found buffer))))))
	 (setq cmd (format "r %s" jong-c-gud-args))
	 (message "the messgae : %s" cmd)
	 (if (equal target-buffer nil)
		  (progn
		    (setq target-buffer (current-buffer))
		    (jong-common-send-command-to-buffer cmd))
	   (jong-common-send-command-to-buffer cmd)
	   )
	 )
  )


(defun jong-c-find-cmake-build (&optional target-dir)
  (interactive)
  (let ((parent-dir))
    (when (string= target-dir nil)
	   (setq target-dir default-directory))
    (if (file-exists-p (format "%s/CMakeLists.txt" target-dir ))
        (with-current-buffer (get-buffer-create jong-c-output-buffer)
		    (shell-command (format "cd \"%s\"; cmake .; make" target-dir)
                         (current-buffer) (current-buffer))
		    (display-buffer (current-buffer)))
	   (unless (string= "/" target-dir)
        (setq parent-dir (file-name-directory (directory-file-name target-dir)))
        (jong-c-find-cmake-build parent-dir)
        ))
    )
  )


(defun jong-c-set-bin-name ()
  (interactive)
  (setq jong-c-bin-name (read-string "Set binary name to run : " ))
  (message "Next jong-c-run-project()'s command is \"%s\"" jong-c-bin-name))


(defun jong-c-run-project (&optional target-dir)
  (interactive)
  (let ((parent-dir))
    (when (string= jong-c-bin-name nil)
	   (progn
        (message "Not setted jong-c-bin-name variable : %s" jong-c-bin-name)
        nil))
    (when (string= target-dir nil)
	   (setq target-dir default-directory))
    (if (file-exists-p (format "%s/%s" target-dir jong-c-bin-name))
        (with-current-buffer (get-buffer-create jong-c-output-buffer)
		    (shell-command (format "cd \"%s\"; ./%s" target-dir jong-c-bin-name)
                         (current-buffer) (current-buffer))
		    (display-buffer (current-buffer)))
	   (unless (string= "/" target-dir)
        (setq parent-dir (file-name-directory (directory-file-name target-dir)))
        (jong-c-run-project parent-dir))
	   )
    )
  )


(defun jong-c-insert-predfine ()
  (interactive)
  (let ((filename buffer-file-name) predefined extension)
    (setq filename (file-name-nondirectory filename))
    (when (not (string-empty-p filename))
	   (setq extension (file-name-extension filename))
	   (if (or (string= extension "h") (string= extension "hpp"))
		    (progn
            (setq predefined (upcase (format "_%s_" (replace-regexp-in-string "\\." "_" filename))))
            (setq predefined (upcase (format "%s" (replace-regexp-in-string "\\-" "_" predefined))))
            (insert (format "#ifndef %s\n" predefined))
            (insert (format "#define %s\n" predefined))
            (insert (format "#endif\n")))
        (message "%s" "The file was not a C header file...")))
    ))


(require 'compile)
(add-hook 'c-mode-common-hook
		    (lambda ()
            (unless (file-exists-p "Makefile")
			     (set (make-local-variable 'compile-command)
				       ;; emulate make's .c.o implicit pattern rule, but with
				       ;; different defaults for the CC, CPPFLAGS, and CFLAGS
				       ;; variables:
				       ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
				       (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s -c -o %s.o %s %s %s"
                             (or (getenv "CC") "gcc")
                             (file-name-sans-extension file)
                             (or (getenv "CPPFLAGS") "-DDEBUG=9")
                             (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
                             file))))))

(defun jo-compile-cmake ()
  (interactive)
  (if (file-exists-p "CMakeLists.txt")
	   (progn
        (if (get-buffer "*compilation*")
            (progn
			     (delete-window-on (get-buffer "*compilation*"))
			     (kill-buffer "*compilation*")))
        (compile "cmake . && ls ./Makefile && make -k"))
    (message "%s" "Couldnt find CMakeList.txt")))


(use-package smart-compile
  :ensure t)
(with-eval-after-load 'smart-compile
  (add-hook 'c-mode-common-hook
            (lambda ()
			     )))

(use-package rtags
  :ensure t)
(with-eval-after-load 'rtags
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (global-company-mode)
  (rtags-enable-standard-keybindings)
  (add-hook 'rtags-jump-hook (lambda ()
							          (push-mark (point))))
  
  (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'objc-mode-hook 'rtags-start-process-unless-running)
  (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))

  (define-key rtags-mode-map (kbd "<C-return>") 'rtags-select-other-window)
  )

(use-package company-rtags
  :ensure t)
(with-eval-after-load 'company-rtags
  (eval-after-load 'company
    '(add-to-list
	   'company-backends 'company-rtags)))

(use-package cmake-ide
  :ensure t)
(with-eval-after-load 'cmake-ide
  (lambda()
    (cmake-ide-setup)))


;; Set linux indent style
(defvar c-default-style)
(defvar c-basic-offset)

;; Add flycheck c++ modep
(add-hook 'c++-mode-hook (lambda ()
						         (setq flycheck-gcc-language-standard "c++11")))

(add-hook 'c-mode-common-hook (lambda ()
                                (setq c-default-style "linux")
                                (setq-default indent-tabs-mode nil)
                                (setq-default tab-width 3)
                                (setq c-basic-offset 3)))


(use-package ggtags
  :ensure t)

(eval-after-load "ggtags"
  '(progn
     (define-key ggtags-mode-map (kbd "C-c g .") (lambda ()
												               (interactive)
												               (evil--jumps-push)
												               (call-interactively 'ggtags-find-definition)))
     (define-key ggtags-mode-map (kbd "C-c g ,") (lambda ()
												               (interactive)
												               (evil--jumps-push)
												               (call-interactively 'ggtags-find-reference)))))

(use-package helm-gtags
  :ensure t)
(add-hook 'c-mode-common-hook
		    (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
			     (ggtags-mode 1))))


(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)


(use-package modern-cpp-font-lock
  :ensure t)
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

(add-hook 'c-mode-common-hook
		    (lambda()
            (local-set-key (kbd "C-c j p") 'jong-c-insert-predfine)
            (local-set-key (kbd "C-c c c") 'jong-c-find-cmake-build)
            (local-set-key (kbd "C-c r s") 'jong-c-set-bin-name)
            (local-set-key (kbd "C-c r r") 'jong-c-run-project)
            (local-set-key (kbd "C-c c m") 'jo-compile-cmake)
            (local-set-key (kbd "C-S-g") 'close-compilation-window)
            (local-set-key (kbd "C-c f f") 'ff-find-other-file)))

;; (defun enable-autoreload-for-dir-locals ()
;; (when (and (buffer-file-name)
;; (equal dir-locals-file
;; (file-name-nondirectory (buffer-file-name))))
;; (add-hook (make-variable-buffer-local 'after-save-hook)
;; 'my-reload-dir-locals-for-all-buffer-in-this-directory)))



;; Set key bindings
;; (eval-after-load "helm-gtags"
;;   '(progn
;;      (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
;;      (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
;;      (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
;;      (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
;;      (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;;      (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
;;      (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))


(provide 'jong-c)
