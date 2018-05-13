;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; c develope environments ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set linux indent style
(defvar c-default-style)
(defvar c-basic-offset)

(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(setq c-default-style "linux")
(setq c-basic-offset 4)

(defun jyc-copy-init-cpp-project()
  (interactive)
  (dired-copy-file-recursive "~/.emacs.d/jongyoungcha/init_cpp_project/" default-directory nil nil nil 'always))

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
  (add-hook 'rtags-jump-hook 'evil--jumps-push)
  (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'objc-mode-hook 'rtags-start-process-unless-running)
  (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))

  (define-key rtags-mode-map (kbd "<C-return>") 'rtags-select-other-window)
  )

(use-package function-args
  :ensure t
  :config
  (function-args-mode t))

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

(add-hook 'c-mode-common-hook
		  (lambda()
			(local-set-key (kbd "C-c c c") 'compile)
			(local-set-key (kbd "C-g") 'kill-temporary-buffers)
			(local-set-key (kbd "C-S-g") 'close-compilation-window)
			(local-set-key (kbd "C-c f f") 'ff-find-other-file)
			(defun enable-autoreload-for-dir-locals ()
			  (when (and (buffer-file-name)
						 (equal dir-locals-file
								(file-name-nondirectory (buffer-file-name))))
				(add-hook (make-variable-buffer-local 'after-save-hook)
						  'my-reload-dir-locals-for-all-buffer-in-this-directory)))))

(provide 'jong-c)


