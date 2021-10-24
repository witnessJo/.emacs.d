;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; c develope environments ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar jong-c-output-buffer "*jong-c-output*" "Jong c language output buffer.")
(add-to-list #'jong-kill-buffer-patterns jong-c-output-buffer)


(defvar jong-c-bin-name nil)
;; "Jong c language run command."
;; :type 'string)

(defvar jong-c-gud-buffer-name nil)
;; "Jong c gud-buffer name."
;; :type 'string)

(defvar jong-c-gud-args nil)
;; "Jong c debug-mode arguments."
;; :type 'string)

;; (define-derived-mode jong-c-gud-mode  gud-mode "jong-gud-mode"
;; (setq font-lock-defaults '(modern-c++-font-lock-keywords)))

(with-no-warnings (add-to-list 'projectile-globally-ignored-directories  ".ccls-cache"))

;; (defun jong-c-gud ()
;; (interactive)
;; (call-interactively 'gud-gdb)
;; (jong-c-gud-mode))

;; (defun jong-c-gud-set-args ()
;; (interactive)
;; (let ((target-buffer nil)
;; (cmd nil))
;; Set gud-gdb arguments.
;; (setq jong-c-gud-args (read-string "jong-gud-mode args : "))
;; (setq target-buffer (get-buffer jong-c-gud-buffer-name))

;; When buffer-name is existing, Get target buffer to send.
;; (when (string= "" jong-c-gud-buffer-name)
;; (setq target-buffer (catch 'found
;; (dolist (buffer (buffer-list))
;; (when (string= jong-c-gud-buffer-name (buffer-name buffer))
;; (throw 'found buffer))))))
;; (setq cmd (format "r %s" jong-c-gud-args))
;; (message "the messgae : %s" cmd)
;; (if (equal target-buffer nil)
;; (progn
;; (setq target-buffer (current-buffer))
;; (jong-common-send-command-to-buffer cmd))
;; (jong-common-send-command-to-buffer cmd)
;; )
;; )
;; )


(defun jong-cc-make-project ()
  (interactive)
  (let ((project-name	(read-string "Project name : "))
		(cmake-path)
		(main-cpp-path))
	(mkdir project-name)
	(f-touch (format "%s/%s" project-name ".projectile"))
	(f-touch (setq cmake-path (format "%s/%s" project-name "CMakeLists.txt")))
	(jong-cc-put-cmake-default cmake-path)
	(f-touch (setq main-cpp-path (format "%s/%s" project-name "main.cpp")))
	(jong-cc-put-main-cpp-default main-cpp-path)
	(projectile-add-known-project (format "%s%s" default-directory project-name)))
  )

(defun jong-cc-put-cmake-default (cmake-path)
  "Write CMake template data to the file.
CMAKE-PATH is the cmake file path."
  (if (with-temp-file cmake-path
		(insert "cmake_minimum_required(VERSION 3.10)\n")
		(insert "set(CMAKE_CXX_FLAGS \"-std=c++11 -O0 -ggdb\")\n")
		(insert "include_directories()\n")
		(insert "add_executable(main main.cpp)\n"))
	  (message "CMakeLists.txt file was not existing... (%s)", cmake-path)
	)
  )

(defun jong-cc-put-main-cpp-default (main-cpp-path)
  "Write CMake template data to the file.
MAIN-CPP-PATH is the cmake file path."
  (if (with-temp-file main-cpp-path
		(insert "#include <iostream>\n")
		(insert "\n")
		(insert "using namespace std;\n")
		(insert "\n")
		(insert "int main(int argc, char* argv[]){\n")
		(insert "\treturn 0;\n")
		(insert "}\n"))
	  (message "main.cpp file was not existing... (%s)" main-cpp-path)

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
			;; add additional flycheck path
			(setq flycheck-clang-include-path (list (expand-file-name "/usr/local/Cellar/ffmpeg/4.2.2_2/include")))
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

(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode) .
		 (lambda ()
		   (require 'ccls)
		   (setq lsp-ui-sideline-enable nil)
		   (setq lsp-ui-doc-enable nil)
		   (lsp)
		   (setq company-auto-complete nil)
		   )))

(setq ccls-executable "/usr/local/bin/ccls")

(use-package cmake-ide
  :ensure t)
(with-eval-after-load 'cmake-ide
  (lambda()
	(cmake-ide-setup)))


(use-package modern-cpp-font-lock
  :ensure t
  :config
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))


(defun jong-c-gdb()
  (interactive)
  (call-interactively 'gdb)
  (gdb-many-windows))


(defun jong-c-setting-coding-style()
  "Setting environment and key bindings."
  (defvar c-default-style)
  (defvar c-basic-offset))

;; (setq c-default-style "linux")
;; (setq-default indent-tabs-mode t
;; tab-width 4))
;; c-basic-offset 2))

;; (jong-c-setting-coding-style)

;; Set linux indent style
(defun jong-c-setting-environment()
  "Setting environment and key bindings."
  
  ;; Set linux indent style
  ;; (setq-local eldoc-documentation-function nil)
  ;; (setq-local eldoc-documentation-function #'rtags-eldoc)

  ;; (lsp-ui-sideline-mode)
  ;; (lsp-ui-doc-mode)
  
  ;; (rtags-start-process-unless-running)
  
  (flymake-mode 0)
  
  (local-set-key (kbd "C-c j p") 'jong-c-insert-predfine)
  (local-set-key (kbd "C-S-g") 'close-compilation-window)
  (local-set-key (kbd "C-c f f") 'ff-find-other-file)
  (local-set-key (kbd "C-c r .") (lambda()
								   (interactive)
								   ;; (call-interactively 'rtags-find-symbol-at-point)
								   (call-interactively 'lsp-find-definition)
								   (jong-common-ring-insert)))
  (local-set-key (kbd "C-c r ,") 'lsp-find-references)
  (local-set-key (kbd "C-c r r") 'lsp-rename)
  (local-set-key (kbd "C-c r l") 'helm-imenu)
  (local-set-key (kbd "M-,") 'jong-common-ring-goto-prev)
  (local-set-key (kbd "M-.") 'jong-common-ring-goto-next)
  )


;; Add flycheck c++ modep
(add-hook 'c-initialization-hook 'jong-c-setting-coding-style)
(add-hook 'c-mode-hook 'jong-c-setting-environment)
(add-hook 'c++-mode-hook 'jong-c-setting-environment)
(add-hook 'objc-mode-hook 'jong-c-setting-environment)

(add-hook 'c++-mode-hook (lambda ()
						   (setq company-auto-complete nil)
						   (setq flycheck-gcc-language-standard "c++14")
						   (setq flycheck-clang-language-standard "c++14")
						   (with-no-warnings (setq company-clang-arguments '("-std=c++14")))))

;; For protecting my eyes...
(add-hook 'objc-mode-hook 'jong-c-setting-environment)

(provide 'jong-cc)
