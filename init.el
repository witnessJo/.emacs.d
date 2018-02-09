;;; Added by Package.el.  This must come before configurations of
;;; installed packages.  Don't delete this line.  If you don't want it,
;;; just comment it out by adding a semicolon to the start of the line.
;;; You may delete these explanatory comments.

(gnutls-available-p)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
			 '("marmalade" . "https://marmalade-repo.org/packages/"))


(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (comint-send-input)))

(defun my-get-selection-length ()
  "Get length of selection"
  (interactive)
  (if (use-region-p)
      (let (pos1 pos2)
		(setq pos1 (region-beginning) pos2 (region-end))
		(- pos2 pos1))
    -1
    )
  )

(defun my-show-selection-length ()
  "Show length of selection"
  (interactive)
  (let (length)
    (setq length (my-get-selection-length))
    (if (equal length -1)
		(message "regions is not activated...")
      (message "length : %d" length)
      )
    )
  )


(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word. With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backword until encountering the end of a word. With argument, dothis that may times."
  (interactive "p")
  (my-delete-word (- arg)))


(defun my-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-06-10"
  (interactive)
  (if current-prefix-arg
      (progn ; not using kill-region because we don't want to include previous kill
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (use-region-p)
               (kill-region (region-beginning) (region-end) t)
             (kill-region (line-beginning-position) (line-beginning-position 2))))))


(defun my-copy-line-or-region ()
  "Copy current line, or text selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2017-07-08"
  (interactive)
  (if current-prefix-arg
      (progn
        (kill-ring-save (point-min) (point-max))
        (message "All visible buffer text copied"))
    (if (use-region-p)
        (progn
          (kill-ring-save (region-beginning) (region-end))
          (message "Active region copied"))
      (if (eq last-command this-command)
          (if (eobp)
              (progn (message "empty line at end of buffer." ))
            (progn
              (kill-append "\n" nil)
              (kill-append
               (buffer-substring-no-properties (line-beginning-position) (line-end-position))
               nil)
              (message "Line copy appended")
              (progn
                (end-of-line)
                (forward-char))))
        (if (eobp)
            (if (eq (char-before) 10 )
                (progn (message "empty line at end of buffer." ))
              (progn
                (kill-ring-save (line-beginning-position) (line-end-position))
                (end-of-line)
                (message "line copied")))
          (progn
            (kill-ring-save (line-beginning-position) (line-end-position))
            (end-of-line)
            (forward-char)
            (message "line copied")))))))


;; When the loading time, the packages will be updated.
(use-package auto-package-update
  :ensure t)
(with-eval-after-load 'auto-package-update
  (lambda()
    (auto-package-update-now)))

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))


(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")
(setenv "PTYHONIOENCODING" "utf-8")


(defun exec-shell-command-with-buffer(cmd temp-buffer-name)
  (interactive)
  (with-output-to-temp-buffer temp-buffer-name
    ;; (shell-command cmd temp-buffer-name "*Massage*")
    (async-shell-command cmd temp-buffer-name temp-buffer-name)
    (pop-to-buffer temp-buffer-name)
    ))

;; add themes
(use-package color-theme-sanityinc-tomorrow :ensure t)
(use-package solarized-theme :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;
										; set the font style  ;
;;;;;;;;;;;;;;;;;;;;;;;
(set-default-coding-systems 'utf-8-unix)

;; set a default font
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono"))

;; specify font for all unicode characters
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;; specify font for chinese characters using default chinese font on linux
(when (member "WenQuanYi Micro Hei" (font-family-list))
  (set-fontset-font t '(#x4e00 . #x9fff) "WenQuanYi Micro Hei" ))

;; (when (eq system-type 'darwin)
;;   (set-face-attribute 'default nil :family "monaco"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  common configurations  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(delete-selection-mode t)

(use-package evil
  :ensure t)
;; (evil-mode 1)
;; (with-eval-after-load 'evil-maps
;;   ;; (fset 'evil-visual-update-x-selection 'ignore)
;;   (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
;;   (define-key evil-normal-state-map (kbd "C-i") 'evil-jump-forward)
;;   (define-key evil-normal-state-map (kbd "C-o") 'evil-jump-backward))

(use-package yasnippet
  :ensure t)
(require 'yasnippet)
(yas-global-mode 1)

(use-package helm
  :ensure t)

(setq helm-split-window-in-side-p t)

(require 'helm-bookmark)

(with-eval-after-load 'helm
  (helm-mode 1)
  (setq helm-candidate-number-limit 500)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "C-x r l") 'helm-bookmarks)
  )

(require 'helm-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; you need ag binary                        ;;
;; $ brew install the_silver_searcher        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package helm-ag
  :ensure t)
(with-eval-after-load 'helm-ag
  (global-set-key (kbd "C-c a g") 'helm-do-ag))

(use-package hungry-delete
  :ensure t)
(global-hungry-delete-mode)


;; reuse a dired list buffer.
(require 'dired)
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "^")
  (lambda () (interactive)
    (find-alternate-file "..")))

;; Remove the key esc esc esc remove other window
(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect
        ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))


(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer.")))))

(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

(defun my-prev-window ()
  (interactive)
  (other-window -1))


(setq confirm-kill-emacs 'y-or-n-p)

(global-set-key (kbd "C-x C-x") 'my-prev-window)
(global-set-key (kbd "C-x C-p") 'other-window)

(global-set-key (kbd "C-S-M-;") 'windmove-left)
(global-set-key (kbd "C-S-M-'") 'windmove-right)

(global-set-key (kbd "C-S-M-[") 'windmove-up)
(global-set-key (kbd "C-<backspace>") 'hungry-delete-backward)
(global-set-key (kbd "C-<deletechar>") 'hungry-delete-forward)

(global-set-key (kbd "M-ESC ESC") 'keyboard-escape-quit)
(global-set-key (kbd "C-M-i") 'evil-jump-forward)
(global-set-key (kbd "C-M-o") 'evil-jump-backward)
(global-set-key (kbd "C-d") 'delete-forward-char)
(global-set-key (kbd "M-d") 'my-delete-word)
(global-set-key (kbd "M-f") 'evil-forward-word-begin)
(global-set-key (kbd "M-b") 'evil-backward-word-end)
(global-set-key (kbd "M-v") 'evil-scroll-up)
(global-set-key (kbd "C-v") 'evil-scroll-down)

(global-set-key (kbd "C-c v y") 'my-copy-linea-or-region)
(global-set-key (kbd "C-c v x") 'my-cut-line-or-region)

(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
  "for every buffer iwth the same `default-directory` as the current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
		(when (equal default-directory dir))
		(my-reload-dir-locals-for-current-buffer)))))


(use-package multi-term
  :ensure)
(with-eval-after-load 'multi-term
  (lambda ()
    (define-key term-raw-map (kbd "C-c C-n") 'multi-term-next)
    (define-key term-raw-map (kbd "C-c C-p") 'multi-term-prev)
    )
  )

(use-package xterm-color
  :ensure)
(require 'ansi-color)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; default setting.
(defun toggle-transparency ()
  "Transparency fr0ame"
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
					((numberp (cdr alpha)) (cdr alpha))
					;; Also handle undocumented (<active> <inactive>) form.

					((numberp (cadr alpha)) (cadr alpha)))
			  100)
		 '(85 . 50) '(100 . 100)))))

(global-set-key (kbd "C-c t") 'toggle-transparency)

(global-set-key (kbd "C-x C-0") 'delete-other-windows-vertically)

;; hide tool bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(defun lispy-parens ()
  "Setup parens display for lisp modes"
  (setq show-paren-delay 0)
  (setq show-paren-style 'parenthesis)

  (show-paren-mode 1)
  (set-face-backgrount 'show-paren-math)
  (show-paren-mode 1)
  (set-face-background 'show-paren-match-face (face-background 'default))
  (if (boundp 'font-lock-comment-face)
      (set-face-foreground 'show-paren-match-face
						   (face-foreground 'font-lock-comment-face))
    (set-face-foreground 'show-paren-match-face
						 (face-foreground 'default)))
  (set-face-attribute 'show-paren-match-face nil :weight 'extra-bold))

(require 'paren)
(set-face-background 'show-paren-match (face-background 'default))
(set-face-foreground 'show-paren-match "#def")
(set-face-attribute 'show-paren-match  nil :weight 'extra-bold)

(show-paren-mode 1)

(setq show-paren-delay 0)

;; reload ~/.emacs.d/init.el file
(defun reload-user-init-file()
  "Load user init.el file"
  (interactive)
  (eval '(load-file user-init-file)))

;; open ~/.emacs.d/init.el
(defun open-init-el()
  "Open user init.el file"
  (interactive)
  (find-file-at-point user-init-file))

(global-set-key (kbd "C-c f e d") 'open-init-el)
(global-set-key (kbd "C-c l e d") 'reload-user-init-file)


(use-package company
  :ensure t)
(require 'company)
(with-eval-after-load 'company
  (setq company-async-timeout 4)
  (add-hook 'after-init-hook 'global-company-mode)
  ;; (defun piotr/company-complete-and-self-inser ()
  ;;   "Complete the current selection and self-insert."
  ;;   (interactive)
  ;;   (company-complete-selection)
  ;;   (call-interactively 'self-insert-command))

  ;; (define-key company-active-map
  ;;   (kbd ".") 'piotr/company-complete-and-self-inser)
  ;; (define-key company-active-map
  ;;   (kbd "SPC")' piotr/company-complete-and-self-inser)
  )

(use-package magit
  :ensure t)

(use-package psvn
  :ensure t)

(use-package projectile
  :ensure t)
(use-package helm-projectile
  :ensure t)

(require 'projectile)


(with-eval-after-load 'projectile
  (projectile-mode t)
  (setq projectile-enable-caching t)
  (setq projectile-globally-ignored-directories (append '(".git") projectile-globally-ignored-directories))
  )
(with-eval-after-load 'helm-projectile
  (setq helm-projectile-fuzzy-match nil))

(setq projectile-completion-system 'helm)

(helm-projectile-on)

(global-set-key (kbd "C-c p p") 'projectile-switch-project)
(global-set-key (kbd "C-c p f") 'projectile-find-file-in-known-projects)
(global-set-key (kbd "C-c w f") 'other-frame)

(use-package exec-path-from-shell
  :ensure t)
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package ido
  :ensure t)
(require 'ido)
(ido-mode t)

(use-package  flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package auto-highlight-symbol
  :ensure t
  :init)
(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)


(require 'files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; elisp develope environments ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'emacs-lisp-mode-hook
		  (lambda()
			(local-set-key (kbd "C-c g g") 'xref-find-definitions)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  python develope environments  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package elpy
  :ensure t)
(with-eval-after-load 'elpy
  (require 'elpy)
  (elpy-enable)
  ;; (elpy-use-ipython)
  (setq elpy-rpc-backend "jedi")
  (setq python-shell-interpreter "ipython")
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  (add-to-list 'python-shell-completion-native-disabled-interpreters "ipython")
  (setq jedi:complete-on-dot t)
  (setq jedi:environment-root "jedi"))

(use-package company-jedi
  :ensure t)
(add-hook 'python-mode-hook
		  (lambda()
			(add-to-list 'company-backend 'company-jedi)))


(global-set-key (kbd "C-c i") 'indent-region)

(use-package anaconda-mode
  :ensure t)
(require 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;; (use-package ob-ipython
;;   :ensure t)
;; (with-eval-after-load 'ob-ipython
;;   (require 'ob-ipython)
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages
;;    '((ipython . t)
;;      ))
;;   )

(defun jyc-run-python ()
  "Use run python program"
  (interactive)
  (compile (concat "python " (buffer-name))))


(defun kill-compilation-buffer ()
  "Kill current buffer unconditionally."
  (interactive)
  (let (
	(buffer-modified-p nil)
	(target-buffers (list "*RTags*" "*compilation*"))
	(current-buffer))

	(while target-buffers
		(when (get-buffer (setq current-buffer (pop target-buffers)))
			(kill-buffer current-buffer))
		)
	(if (not (equal projectile-project-name nil))
		(when (get-buffer (setq current-buffer (format "%s-%s" "*compilation*" projectile-project-name)))
			(kill-buffer current-buffer)))
	))
	

(defun close-compilation-window ()
  "Close the window having compilation buffer"
  (interactive)
  (if (not (equal projectile-project-name nil))
      (delete-windows-on (format "%s-%s" "*compilation*" projectile-project-name)))
    ;; (delete-windows-on "*compilation*")
  	(delete-windows-on "*RTags")
	)
	

(add-hook 'python-mode-hook
		  (lambda ()
			(local-set-key (kbd "C-c g g") 'anaconda-mode-find-definitions)
			(local-set-key (kbd "C-c c c") 'jyc-run-python)
			(local-set-key (kbd "C-g") 'kill-compilation-buffer)
			(local-set-key (kbd "C-S-g") 'close-compilation-window)
			(linum-mode t)
			))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; c develope environments ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set linux indent style
(defvar c-default-style)
(defvar c-basic-offset)
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
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

;; (define-key c-mode-base-map (kbd "<f5>") 'c/c++-simple-compile-and-exec)

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

(use-package company-rtags
  :ensure t)
(with-eval-after-load 'company-rtags
  (eval-after-load 'company
    '(add-to-list
      'company-backends 'company-rtags)))

;; (use-package helm-rtags
;;   :ensure t)
;; (with-eval-after-load 'helm-rtags
;;   (setq rtags-use-helm t))

;; (use-package irony
;;   :ensure t)
;; (with-eval-after-load 'irony
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'c-mode-hook 'irony-mode)
;;   (add-hook 'objc-mode-hook 'irony-mode))

;; (defun my-irony-mode-hook ()
;;   "My irony mode hook"
;;   (define-key irony-mode-map [remap completion-at-point]
;;     'irony-completion-at-point-async)
;;   (define-key irony-mode-map [remap complete-symbol]
;;     'irony-completion-at-point-async))

;; (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;; (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; (use-package company-irony
;;   :ensure t)
;; (with-eval-after-load 'company-irony
;;   (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
;;   (setq company-backends (delete 'company-semantic company-backends))
;;   (setq company-idle-delay 0)
;;   (define-key c++-mode-map (kbd "C-c <C-tab>") 'company-complete)
;;   (define-key c-mode-map (kbd "C-c <C-tab>") 'company-complete)
;;   )

;; (use-package company-irony-c-headers
;;   :ensure t)
;; (require 'company-irony-c-headers)

;; (eval-after-load 'company
;; '(add-to-list
;;     'company-backends '(company-irony-c-headers company-irony)))

;; (with-eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

(use-package cmake-ide
  :ensure t)
(with-eval-after-load 'cmake-ide
  (lambda()
    (cmake-ide-setup)))

(add-hook 'c-mode-common-hook
		  (lambda()
			(local-set-key (kbd "C-c c c") 'compile)
			(local-set-key (kbd "C-g") 'kill-compilation-buffer)
			(local-set-key (kbd "C-S-g") 'close-compilation-window)
			(local-set-key (kbd "C-c f f") 'ff-find-other-file)
			(defun enable-autoreload-for-dir-locals ()
			  (when (and (buffer-file-name)
						 (equal dir-locals-file
								(file-name-nondirectory (buffer-file-name))))
				(add-hook (make-variable-buffer-local 'after-save-hook)
					  'my-reload-dir-locals-for-all-buffer-in-this-directory)))))


(use-package org
  :ensure t)
  

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
	("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "789ca211efb5c1c679293897339aa3e0691c9a45e15c72e1c2b420c05b41f3c9" default)))
 '(elpy-rpc-large-buffer-size 8192)
 '(fci-rule-color "#003f8e")
 '(package-selected-packages
   (quote
	(org-mode psvn hungry-delete bison-mode magit helm-ag rtags-helm irony ob-ipython ein sanityinc-tomorrow-blue company projectile auto-complete company-mode evil use-package helm)))
 '(python-shell-interpreter "ipython")
 '(safe-local-variable-values
   (quote
	((projectile-project-compilation-cmd . "cmake -DCMAKE_BUILD_TYPE=DEBUG -DCMAKE_EXPORT_COMPILE_COMMANDS=ON .; make")
	 (projectile-project-compilation-cmd . "cmake -DCMAKE_BUILD_TYPE=DEBUG .; make")
	 (projectile-project-compilation-cmd . "cmake . -DCMAKE_BUILD_TYPE=DEBUG; make")
	 (projectile-project-compilation-cmd . "cmake . DCMAKE_BUILD_TYPE=DEBUG; make")
	 (projectile-project-root . "./")
	 (projectile-project-name . "ants")
	 (projectile-project-compilation-cmd . "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -DCMAKE_BUILD_TYPE=Debug .;make")
	 (projectile-project-run-cmd . "rm -rf ./wv_shm_log;./wv_shm_test")
	 (projectile-project-compilation-cmd . "cmake CMAKE_EXPORT_COMPILE_COMMANDS=1 .;make")
	 (projectile-project-compilation-cmd . "cmake .;make")
	 (projectile-project-run-cmd . "./wv_shm_test")
	 (projectile-project-root . "/Users/joyeongchan/projects/fuckerSQL/")
	 (eval defun jyc-sql-compile-exec nil
		   (interactive)
		   (exec-shell-command-with-buffer
			(format "python %s/build.py --bin %s --home %s --debug --cmd --exec --args %s " projectile-project-root projectile-project-name projectile-project-root jyc-program-args)
			(format "%s-%s" "*compilation*" projectile-project-name))
		   (switch-to-buffer-other-window
			(other-buffer
			 (current-buffer)
			 1)))
	 (jyc-program-args . "./test_sql.txt")
	 (eval defun jyc-sql-compile-exec nil
		   (interactive)
		   (exec-shell-command-with-buffer
			(format "python %s/build.py --bin %s --home %s --debug --cmd --exec " projectile-project-root projectile-project-name projectile-project-root)
			(format "%s-%s" "*compilation*" projectile-project-name))
		   (switch-to-buffer-other-window
			(other-buffer
			 (current-buffer)
			 1)))
	 (eval defun jyc-sql-compile nil
		   (interactive)
		   (exec-shell-command-with-buffer
			(format "python %s/build.py --bin %s --home %s --debug --cmd " projectile-project-root projectile-project-name projectile-project-root)
			(format "%s-%s" "*compilation*" projectile-project-name))
		   (compilation-minor-mode t)
		   (switch-to-buffer-other-window
			(other-buffer
			 (current-buffer)
			 1)))
	 (projectile-project-name . "fucker_sql")
	 (projectile-project-root . "/Users/joyeongchan/projects/jyc-sql/")
	 (projectile-project-name . "jyc_sql")
	 (eval defun jyc-sql-compile-exec nil
		   (interactive)
		   (exec-shell-command-with-buffer
			(format "python %s/build.py --bin %s --home %s --debug --cmd " projectile-project-root projectile-project-name projectile-project-root)
			(format "%s-%s" "*compilation*" projectile-project-name))
		   (switch-to-buffer-other-window
			(other-buffer
			 (current-buffer)
			 1)))
	 (eval defun jyc-sql-compile nil
		   (interactive)
		   (message
			(format "python %s/build.py --bin %s --home %s --debug --cmd " projectile-project-root projectile-project-name projectile-project-root))
		   (exec-shell-command-with-buffer
			(format "python %s/build.py --bin %s --home %s --debug --cmd " projectile-project-root projectile-project-name projectile-project-root)
			(format "%s-%s" "*compilation*" projectile-project-name))
		   (compilation-minor-mode t)
		   (switch-to-buffer-other-window
			(other-buffer
			 (current-buffer)
			 1)))
	 (projectile-project-root . "/Users/joyeongchan/projects/jyc-cheat/jyc-cheat-client/")
	 (eval defun jyc-sql-compile nil
		   (interactive)
		   (exec-shell-command-with-buffer
			(format "python %s/build.py --debug --cmd " projectile-project-root)
			(format "%s-%s" "*compilation*" projectile-project-name))
		   (compilation-minor-mode t)
		   (switch-to-buffer-other-window
			(other-buffer
			 (current-buffer)
			 1)))
	 (eval local-set-key
		   (kbd "<f6>")
		   (quote jyc-compile-exec))
	 (eval local-set-key
		   (kbd "<f5>")
		   (quote jyc-compile))
	 (eval defun jyc-compile-exec nil
		   (interactive)
		   (exec-shell-command-with-buffer
			(format "python %s/build.py --debug --cmd --exe" projectile-project-root)
			(format "%s-%s" "*compilation*" projectile-project-name))
		   (compilation-minor-mode t)
		   (switch-to-buffer-other-window
			(other-buffer
			 (current-buffer)
			 1)))
	 (eval defun jyc-compile nil
		   (interactive)
		   (shell-command
			(format "python %s/build.py --debug --cmd" projectile-project-root))
		   (switch-to-buffer-other-window
			(other-buffer
			 (current-buffer)
			 1)))
	 (projectile-project-root . "~/projects/jyc-cheat/jyc-cheat-test/ncurses/")
	 (projectile-project-name . "jyc_cheat_test_ncurses")
	 (projectile-project-name . "jyc_cheat_client")
	 (projectile-project-name . "jyc_cheat_server")
	 (eval defun jyc-sql-compile-exec nil
		   (interactive)
		   (exec-shell-command-with-buffer
			(format "python %s/build.py --debug --cmd --exe" projectile-project-root)
			(format "%s-%s" "*compilation*" projectile-project-name))
		   (compilation-minor-mode t)
		   (switch-to-buffer-other-window
			(other-buffer
			 (current-buffer)
			 1)))
	 (projectile-project-name . "jyc-cheat-client")
	 (eval defun jyc-sql-compile-exec nil
		   (interactive)
		   (exec-shell-command-with-buffer
			(format "python %s/build.py --debug --cmd --exe" projectile-project-root)
			(format "%s-%s" "*compilation*" projectile-project-name))
		   (switch-to-buffer-other-window
			(other-buffer
			 (current-buffer)
			 1)))
	 (projectile-project-name . "jyc-cheat-server")
	 (eval defun jyc-sql-compile-exec nil
		   (interactive)
		   (exec-shell-command-with-buffer
			(format "python %s/build.py --debug --cmd --exe" projectile-project-root)
			"*compilation*")
		   (compilation-minor-mode t)
		   (switch-to-buffer-other-window
			(other-buffer
			 (current-buffer)
			 1)))
	 (eval defun jyc-sql-compile nil
		   (interactive)
		   (shell-command
			(format "python %s/build.py --debug --cmd" projectile-project-root))
		   (compilation-minor-mode t)
		   (switch-to-buffer-other-window
			(other-buffer
			 (current-buffer)
			 1)))
	 (eval defun jyc-sql-compile-exec nil
		   (interactive)
		   (exec-shell-command-with-buffer
			(format "python %s/build.py --debug --cmd --exe" projectile-project-root)
			"*compilation*")
		   (compilation-mode)
		   (switch-to-buffer-other-window
			(other-buffer
			 (current-buffer)
			 1)))
	 (eval defun jyc-sql-compile nil
		   (interactive)
		   (shell-command
			(format "python %s/build.py --debug --cmd" projectile-project-root))
		   (compilation-mode)
		   (switch-to-buffer-other-window
			(other-buffer
			 (current-buffer)
			 1)))
	 (eval defun jyc-sql-compile-exec nil
		   (interactive)
		   (shell-command
			(format "python %s/build.py --debug_mode --cmd_export --execute" default-directory)))
	 (eval defun jyc-sql-compile nil
		   (interactive)
		   (shell-command
			(format "python %s/build.py --debug_mode --cmd_export" default-directory)))
	 (projectile-project-root . "~/projects/jyc-cheat/jyc-cheat-client/")
	 (projectile-project-root . "~/projects/jyc-cheat/jyc-cheat-server/")
	 (eval defun jyc-sql-compile-exec nil
		   (interactive)
		   (exec-shell-command-with-buffer
			(format "python %s/build.py --debug --cmd --exe" projectile-project-root)
			"*compilation*")
		   (switch-to-buffer-other-window
			(other-buffer
			 (current-buffer)
			 1)))
	 (projectile-project-root . "~/projects/jyc-cheat/server")
	 (eval defun jyc-sql-compile-exec nil
		   (interactive)
		   (exec-shell-command-with-buffer
			(format "python %s/build.py --debug --cmd --exe --args \"%s/test_sql.txt\"" projectile-project-root projectile-project-root)
			"*compilation*")
		   (switch-to-buffer-other-window
			(other-buffer
			 (current-buffer)
			 1)))
	 (eval defun jyc-sql-compile nil
		   (interactive)
		   (shell-command
			(format "python %s/build.py --debug --cmd" projectile-project-root))
		   (switch-to-buffer-other-window
			(other-buffer
			 (current-buffer)
			 1)))
	 (eval defun jyc-sql-compile-exec nil
		   (interactive)
		   (exec-shell-command-with-buffer
			(format "python %s/build.py --debug --cmd --exe --args \"%s/test_sql.txt\"" projectile-project-root projectile-project-root)
			"*compilation*")
		   (switch-to-buffer
			(other-buffer
			 (current-buffer)
			 1)))
	 (eval defun jyc-sql-compile nil
		   (interactive)
		   (shell-command
			(format "python %s/build.py --debug --cmd" projectile-project-root))
		   (switch-to-buffer
			(other-buffer
			 (current-buffer)
			 1)))
	 (eval defun jyc-sql-compile-exec nil
		   (interactive)
		   (exec-shell-command-with-buffer
			(format "python %s/build.py --debug --cmd --exe --args \"%s/test_sql.txt\"" projectile-project-root projectile-project-root)
			"*compilation*"))
	 (eval defun jyc-sql-compile-exec nil
		   (interactive)
		   (exec-shell-command-with-buffer
			(format "python %s/build.py --debug --cmd --exe --args \"./test_sql.txt\"" projectile-project-root projectile-project-root)
			"*compilation*"))
	 (eval defun jyc-sql-compile-exec nil
		   (interactive)
		   (exec-shell-command-with-buffer
			(format "python %s/build.py --debug --cmd -exe --args \"./test_sql.txt\"" projectile-project-root projectile-project-root)
			"*compilation*"))
	 (projectile-project-root . "~/projects/jyc-sql/")
	 (eval defun jyc-sql-compile-exec nil
		   (interactive)
		   (exec-shell-command-with-buffer
			(format "python %s/build.py --debug --cmd --args \"./test_sql.txt\"" projectile-project-root projectile-project-root)
			"*compilation*"))
	 (eval defun jyc-sql-compile nil
		   (interactive)
		   (shell-command
			(format "python %s/build.py --debug --cmd" projectile-project-root)))
	 (projectile-project-root . "~/projects/jyc-sql")
	 (eval defun jyc-sql-compile-exec nil
		   (interactive)
		   (exec-shell-command-with-buffer
			(format "python %s/build.py --debug --cmd --exe --args \"./test_sql.txt\"" default-directory)
			"*compilation*"))
	 (eval defun jyc-sql-compile-exec nil
		   (interactive)
		   (shell-command
			(format "python %s/build.py --debug --cmd --exe" default-directory)))
	 (eval defun jyc-sql-compile nil
		   (interactive)
		   (shell-command
			(format "python %s/build.py --debug --cmd" default-directory)))
	 (eval local-set-key
		   (kbd "<f6>")
		   (quote jyc-sql-compile-exec))
	 (eval local-set-key
		   (kbd "<f5>")
		   (quote jyc-sql-compile))
	 (eval defun jyc-sql-compile-exec nil
		   (interactive)
		   (shell-command "python build.py --debug_mode --cmd_export --execute"))
	 (eval defun jyc-sql-compile nil
		   (interactive)
		   (shell-command "python build.py --debug_mode --cmd_export"))
	 (cmake-ide--build-dir-var . "./"))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
	((20 . "#ff9da4")
	 (40 . "#ffc58f")
	 (60 . "#ffeead")
	 (80 . "#d1f1a9")
	 (100 . "#99ffff")
	 (120 . "#bbdaff")
	 (140 . "#ebbbff")
	 (160 . "#ff9da4")
	 (180 . "#ffc58f")
	 (200 . "#ffeead")
	 (220 . "#d1f1a9")
	 (240 . "#99ffff")
	 (260 . "#bbdaff")
	 (280 . "#ebbbff")
	 (300 . "#ff9da4")
	 (320 . "#ffc58f")
	 (340 . "#ffeead")
	 (360 . "#d1f1a9"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
