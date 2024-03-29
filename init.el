;;; Avdded by Package.el.  This must come before configurations of
;;; installed packages.  Don't delete this line.  eIf you don't want it,
;;; just comment it out by adding a semicolon to the start of the line.
;;; You may delete these explanatory comments.

(gnutls-available-p)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package--check-signature nil)

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
						 ("melpa" . "https://melpa.org/packages/")
						 ("elpa" . "https://mirrors.163.com/elpa/gnu/")))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(server-start)
(add-to-list 'load-path "~/.emacs.d/jongyoungcha")

(when (memq window-system '(mac ns))
  (shell-command "brew Tap homebrew/cask-fonts && brew install --cask font-source-code-pro")
  (exec-path-from-shell-copy-env "GOROOT")
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "PATH")
  )

(setq history-length 25)
(savehist-mode t)
(save-place-mode t)
(setq custom-file (locate-user-emacs-file "custom-vars.el"))

(use-package auto-package-update
  :ensure t
  :config
  ;; Yes, please delete the old version on updates.
  (setq auto-package-update-delete-old-versions t))

(global-font-lock-mode t)
(transient-mark-mode 1)

(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq eldoc-idle-delay 0.05)

(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")
(setenv "PTYHONIOENCODING" "utf-8")

(defun exec-shell-command-with-buffer(cmd temp-buffer-name)
  (interactive)
  (with-output-to-temp-buffer temp-buffer-name
	(async-shell-command cmd temp-buffer-name temp-buffer-name)
	(pop-to-buffer temp-buffer-name)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  common configurations  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reuse a dired list buffer.
(require 'dired)
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "^")
            (lambda () (interactive)
              (find-alternate-file "..")))

;; Remove the key esc esc esc remove other window
(defadvice keyboard-escape-quit (around jong-keyboard-escape-quit activate)
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

(setq confirm-kill-emacs 'y-or-n-p)
(setq mark-ring-max 8)
(setq global-mark-ring-max 8)
(setq set-mark-command-repeat-pop t)

(defun jong-forward-line (number)
  (interactive)
  (let ((curr-column (- (point) (progn (beginning-of-line)
									   (point))))
		(max-column)
		(target-column))
	(when (not (numberp number))
	  (error "Number was not Integer"))

	(forward-line number)
	;; (goto-char (+ (point) curr-column))
	(setq target-column (+ (point) curr-column))
	(setq max-column (progn (end-of-line)
							(point)))
	(if (> target-column max-column)
		(goto-char max-column) 
	  (goto-char target-column))
	;; (recenter-top-bottom (line-number-at-pos))
    ;; (recenter (line-number-at-pos))
    ))


(defun pop-local-or-global-mark ()
  "Pop to local mark if it exists or to the global mark if it does not."
  (interactive)
  (if (mark t)
	  (pop-to-mark-command)
	(pop-global-mark)))

;; default setting.
(defun toggle-transparency ()
  "Transparency frame."
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

;; (global-set-key (kbd "C-c t") 'toggle-transparency)
(global-set-key (kbd "C-x C-0") 'delete-other-windows-vertically)

;; hide tool bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(defun reload-user-init-file()
  "Load user init.el file"
  (interactive)
  (eval '(load-file user-init-file)))

;; open ~/.emacs.d/init.el
(defun open-init-el()
  "Open user init.el file"
  (interactive)
  (find-file-at-point user-init-file))

(defun open-dot-jongyoungcha()
  (interactive)
  (find-file-read-only (format "%s/.jongyoungcha" (getenv "HOME"))))

(defun jo-set-projectile-run-command ()
  "Read user input commajksldfnd and set ectile-project-run-cmd'."
  (interactive)
  (let (user-input)
	(if (nt (equal "" (setq user-input (read-string "Enter the command : "))))
		(progn
		  (setq projectile-project-run-cmd user-input)
		  (message "Changed projectile-project-run-cmd as %s" user-input))
	  (message "The command was empty..."))
	))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
	(exec-path-from-shell-initialize)))

(use-package ido
  :ensure t
  :config
  :init
  (ido-mode t)
  (setq ido-enable-flex-matching t))


(use-package  flycheck
  :ensure t
  :config
  )

(require 'files)

(setq split-height-threshold (+ (/ (frame-height) 2) 1))
(setq split-width-threshold  (/ (frame-width) 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; elisp develope environments ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'emacs-lisp-mode-hook
		  (lambda()
			(local-set-key (kbd "C-c g g") 'xref-find-definitions)))

(defun jyc-run-python ()
  "Use run python program"
  (interactive)
  (compile (concat "python " (buffer-name))))

(defcustom  jong-kill-buffer-patterns nil
  "this is patters to kill buffer"
  :type 'list)
(setq jong-kill-buffer-patterns (list "*RTags*"
									  "*compilation*"
									  "*Occur*"
									  "*Help*"
									  "^\*godoc.*"
									  "*Warnings*"
									  "*xref*"
									  "*Node Shell*"
									  "*Google Translate*"
									  "*jong-output*"
									  ))

(defun jong-kill-temporary-buffers ()
  "Kill current buffer unconditionally."
  (interactive)
  (dolist (pattern jong-kill-buffer-patterns)
	(dolist (buffer (buffer-list))
	  (when (string-match pattern (buffer-name buffer))
		(kill-buffer buffer))))
  )

(require 'jong-packages)
(require 'jong-journal)
(require 'jong-lsp)
(require 'jong-common)
(require 'jong-project)
(require 'jong-font-locks)
(require 'jong-cursor)
(require 'jong-window)
(require 'jong-buffer)
(require 'jong-code)
(require 'jong-assist)

;; Langauges
(require 'jong-elisp)
(require 'jong-bash)
(require 'jong-tramp)
(require 'jong-scheme)
(require 'jong-cmake)
(require 'jong-cc)
(require 'jong-python)
(require 'jong-rust)
(require 'jong-scala)
(require 'jong-haskell)
(require 'jong-nodejs)
(require 'jong-go)
(require 'jong-java)
(require 'jong-kotlin)
(require 'jong-sql)
(require 'jong-protobuf)
(require 'jong-fabric)

;; Shell
(require 'jong-eshell)

;; ETC files
(require 'jong-makefile)
(require 'jong-yaml)

;; Utils
(require 'jong-org)
(require 'jong-helm)
(require 'jong-network)
(require 'jong-http)
(require 'jong-html)
(require 'jong-dap-debug)
(require 'jong-term)
(require 'jong-bookmark)
(require 'jong-redis)
(require 'jong-git)
(require 'jong-kubernetes)
(require 'jong-plantuml)

(require 'jong-env-setting)
(require 'jong-debug-settings)

(require 'jong-key-bindings)
(require 'jong-shell-util)
(require 'jong-themes)

(require 'jo-bundle)

(global-hl-line-mode t)
;; (set-face-background #'hl-line "#004500")
(setq jong-go-run-command (format "./geth --datadir=~/testnet --verbosity 4 --bootnodes %s --syncmode \"full\" --cache=2048" (getenv "BOOTNODE")))
(setq jong-go-run-default-path "~/goworks/src/github.com/ethereum/go-ethereum/cmd/geth")
(put 'dired-find-alternate-file 'disabled nil)

(load custom-file 'noerror 'nomessage)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-error ((((class color)) (:background "#444444"))))
 '(flymake-warning ((((class color)) (:background "#4444aa")))))
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
