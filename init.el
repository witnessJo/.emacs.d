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


(defun jong-forward-delete-word ()
  "Chan 'forward-delete-word."
  (interactive)
  (let ((target-string "")
		(base-pos 0)
		(fword-pos 0)
		(candidate-pos 0)
		(curr-char)
		)
	(setq curr-char (string (char-after (point))))
	(if (string-match curr-char "[ \n\t] ")
		(call-interactively #'hungry-delete-forward)
	  (progn
		(setq base-pos (point))
		(search-forward-regexp candidate-chars nil 'noerror)
		(setq candidate-pos (point))
		(forward-word)
		(setq fword-pos (point))
		(goto-char base-pos)
		(if (> candidate-pos fword-pos)
			(delete-region base-pos fword-pos)
		  (delete-region base-pos candidate-pos))))
	)
  )


(defun jong-backward-delete-word ()
  "Chan 'backward-delete-word."
  (interactive)
  (let ((target-string "")
		(base-pos 0)
		(bword-pos 0)
		(candidate-pos 0)
		(curr-char)
		)
	(setq curr-char (string (char-after (1- (point)))))
	(if (string-match curr-char "[ \n] ")
		(call-interactively #'hungry-delete-backward)
	  (progn
		(setq base-pos (point))
		(search-backward-regexp candidate-chars nil 'noerror)
		(setq candidate-pos (point))
		(backward-word)
		(setq bword-pos (point))
		(goto-char base-pos)
		(if (> candidate-pos bword-pos)
			(progn
			  (ignore-errors (delete-region candidate-pos base-pos))
			  (goto-char candidate-pos)
			  )
		  (ignore-errors (delete-region (1- bword-pos) base-pos)))
		))
	)
  )


(setq confirm-kill-emacs 'y-or-n-p)
(setq mark-ring-max 8)
(setq global-mark-ring-max 8)
(setq set-mark-command-repeat-pop t)


(global-set-key (kbd "M-;") (lambda () (interactive)
							  (let ((base-pos 0))
								(setq base-pos (point))
								(beginning-of-line)
								(call-interactively 'comment-line)
								(goto-char base-pos)
								(forward-line)
								(indent-for-tab-command)
								)))


(global-set-key (kbd "M-ESC ESC") 'keyboard-escape-quit)
(global-set-key (kbd "C-d") 'delete-forward-char)

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
	(recenter-top-bottom (line-number-at-pos))))


(defun pop-local-or-global-mark ()
  "Pop to local mark if it exists or to the global mark if it does not."
  (interactive)
  (if (mark t)
	  (pop-to-mark-command)
	(pop-global-mark)))


(defun jong-set-mark ()
  (interactive)
  (setq this-command-keys-shift-translated t)
  (if (not (use-region-p))
	  (call-interactively 'set-mark-command)))


(global-set-key (kbd "C-S-f") (lambda () (interactive)
								(jong-set-mark)
								(goto-char (1+ (point)))))

(global-set-key (kbd "C-S-b") (lambda () (interactive)
								(jong-set-mark)
								(goto-char (1- (point)))))

(global-set-key (kbd "C-S-a") (lambda () (interactive)
								(jong-set-mark)
								(beginning-of-line)))

(global-set-key (kbd "C-S-e") (lambda () (interactive)
								(jong-set-mark)
								(end-of-line)))

(global-set-key (kbd "C-S-a") (lambda () (interactive)
								(jong-set-mark)
								(beginning-of-line)))

(global-set-key (kbd "C-S-p") (lambda () (interactive)
								(jong-set-mark)
								(forward-line -1)))

(global-set-key (kbd "C-S-n") (lambda () (interactive)
								(jong-set-mark)
								(forward-line 1)))

(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "C-x p") (lambda() (interactive) (other-window -1)))
(global-set-key (kbd "C-c C-o") 'other-window)

(global-set-key (kbd "C-M-i") (lambda() (interactive) (scroll-other-window -15)))
(global-set-key (kbd "C-M-o") (lambda() (interactive) (scroll-other-window 15)))

(global-set-key (kbd "C-c <") (lambda() (interactive)
								(call-interactively 'eyebrowse-prev-window-config)
								(message "slot : %s" (eyebrowse--get 'current-slot))))

(global-set-key (kbd "C-c >") (lambda() (interactive)
								(call-interactively 'eyebrowse-next-window-config)
								(message "slot : %s" (eyebrowse--get 'current-slot))))

(global-set-key (kbd "C-c w w") (lambda() (interactive)
								  (call-interactively 'eyebrowse-switch-to-window-config-1)
								  (call-interactively 'eyebrowse-switch-to-window-config-2)
								  (call-interactively 'eyebrowse-switch-to-window-config-3)))

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

(global-set-key (kbd "C-c f e d") 'open-init-el)
(global-set-key (kbd "C-c l e d") 'reload-user-init-file)


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


(global-set-key (kbd "C-c p p") 'projectile-switch-project)
(global-set-key (kbd "C-c p f") 'projectile-find-file)
(global-set-key (kbd "C-c p c") 'projectile-compile-project)
(global-set-key (kbd "C-c p r") 'projectile-run-project)
(global-set-key (kbd "C-c p s") 'jo-set-projectile-run-command)
(global-set-key (kbd "C-c w f") 'other-frame)

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
									  "*jong-output*"))

(defun jong-kill-temporary-buffers ()
  "Kill current buffer unconditionally."
  (interactive)
  (dolist (pattern jong-kill-buffer-patterns)
	(dolist (buffer (buffer-list))
	  (when (string-match pattern (buffer-name buffer))
		(kill-buffer buffer))))
  )

(global-set-key (kbd "C-g") (lambda () (interactive)
							  (jong-kill-temporary-buffers)
							  (keyboard-quit)))

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "Directory: ")
  (eshell-command
   (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

	;;;  Jonas.Jarnestrom<at>ki.ericsson.se A smarter
	;;;  find-tag that automagically reruns etags when it cant find a
	;;;  requested item and then makes a new try to locate it.
	;;;  Fri Mar 15 09:52:14 2002
(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo find-tag.
	 If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
	(condition-case err
		ad-do-it
	  (error (and (buffer-modified-p)
				  (not (ding))
				  (y-or-n-p "Buffer is modified, save it? ")
				  (save-buffer))
			 (er-refresh-etags extension)
			 ad-do-it))))

(defun er-refresh-etags (&optional extension)
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)
  (shell-command (format "etags *.%s" (or extension "el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently
	(visit-tags-table default-directory nil)))


(require 'jong-packages)
(require 'jong-env-setting)
(require 'jong-common)
(require 'jong-project)
(require 'jong-cursor)
(require 'jong-window)

;; Langauges
(require 'jong-elisp)
(require 'jong-bash)
(require 'jong-tramp)
(require 'jong-scheme)
(require 'jong-cmake)
(require 'jong-makefile)
(require 'jong-cc)
(require 'jong-python)
(require 'jong-rust)
(require 'jong-scala)
(require 'jong-haskell)
(require 'jong-nodejs)
(require 'jong-go)

;; Utils
(require 'jong-helm)
(require 'jong-network)
(require 'jong-http)
(require 'jong-html)
(require 'jong-dap-debug)
(require 'jong-term)
(require 'jong-grpc)
(require 'jong-swit)
(require 'jong-swit-dotenv1)

;; For Testing
(require 'jong-debug-settings)
(require 'jong-key-bindings)

(setq jong-go-run-command (format "./geth --datadir=~/testnet --verbosity 4 --bootnodes %s --syncmode \"full\" --cache=2048" (getenv "BOOTNODE")))
(setq jong-go-run-default-path "~/goworks/src/github.com/ethereum/go-ethereum/cmd/geth")
(set-cursor-color "#aa4444")
(set-face-background #'hl-line "#004500")
(global-hl-line-mode t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(beacon-color "#ff9da4")
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes '(sanityinc-tomorrow-blue))
 '(custom-safe-themes
   '("285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default))
 '(fci-rule-color "#073642")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(helm-completion-style 'emacs)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   (--map
	(solarized-color-blend it "#002b36" 0.25)
	'("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   '(("#073642" . 0)
	 ("#546E00" . 20)
	 ("#00736F" . 30)
	 ("#00629D" . 50)
	 ("#7B6000" . 60)
	 ("#8B2C02" . 70)
	 ("#93115C" . 85)
	 ("#073642" . 100)))
 '(hl-bg-colors
   '("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00"))
 '(hl-fg-colors
   '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(kubernetes-exec-arguments '("-i" "-t" "--container=server"))
 '(kubernetes-logs-arguments '("--tail=1000" "--container=server"))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4"))
 '(package-selected-packages
   '(aggressive-indent aggresive-indent aggresive-indent-mode magit-delta multiple-cursors evil-mode evil company-lsp company-lspf ivy-posframe counsel-projectile ag counsel go-fill-struct inf-mongo vterm plantuml-mode exwm kubernetes-tramp kubernetes gotest smartparens smartparens-stric protobuf-mode cider lsp-ui yaml-mode xterm-color xref-js2 whitespace-cleanup-mode which-key web-mode use-package undo-tree tide syntax-subword solarized-theme rtags restclient realgud racer prodigy popwin pcap-mode nodejs-repl modern-cpp-font-lock magit log4e js-comint indium hungry-delete helm-xref helm-projectile helm-go-package helm-dash helm-ag google-translate godoctor go-stacktracer go-rename go-guru go-errcheck go-eldoc go-dlv go-direx go-complete go-autocomplete flymake-go flycheck-rust flycheck-haskell exec-path-from-shell ensime elpy elisp-slime-nav elisp-refs dap-mode company-quickhelp company-jedi company-go color-theme-sanityinc-tomorrow cmake-mode cmake-ide clang-format ccls cargo bash-completion avy autopair auto-package-update auto-highlight-symbol anaconda-mode))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(safe-local-variable-values
   '((jong-project-sub-default-dir-3 . "/Users/richard/go/src/demeter/")
	 (jong-project-sub-default-dir-2 . "/Users/richard/go/src/demeter/")
	 (jong-project-sub-default-dir-3 . "/Users/richard/go/src/maat/")
	 (jong-project-sub-default-dir-2 . "/Users/richard/go/src/maat/")
	 (jong-project-sub-default-dir-3 . "/Users/richard/go/src/chandra/")
	 (jong-project-sub-default-dir-2 . "/Users/richard/go/src/chandra/")
	 (jong-project-sub-default-dir-3 . "/Users/richard/go/src/mitra/")
	 (jong-project-sub-default-dir-2 . "/Users/richard/go/src/mitra/")
	 (jong-project-sub-default-dir-3 . "/Users/swit-mac/go/src/swit/swit-grpc-asset-golang/")
	 (jong-project-sub-default-dir-2 . "/Users/swit-mac/go/src/swit/swit-grpc-asset-golang/")
	 (list "testcmd1" "ls" "/home/jongyoungcha/go/src/github.com/jongyoungcha/go-parallel-sample/")
	 (jong-project-sub-default-dir-3 . "/home/jongyoungcha/go/src/github.com/jongyoungcha/go-parallel-sample/")
	 (jong-project-sub-default-dir-2 . "/home/jongyoungcha/go/src/github.com/jongyoungcha/go-parallel-sample/")
	 (jong-project-sub-default-dir-3 . "/Users/swit-mac/go/src/swit/swit-image-cloud-function-golang/")
	 (jong-project-sub-command-2 . "./mockclient")
	 (jong-project-sub-default-dir-2 . "/Users/swit-mac/go/src/swit/swit-image-cloud-function-golang/")
	 (jong-project-sub-default-dir-3 . "/home/jongyoungcha/go/src/swit/swit-image-cloud-function-golang/")
	 (jong-project-sub-default-dir-2 . "/home/jongyoungcha/go/src/swit/swit-image-cloud-function-golang/")
	 (jong-project-cmd-test "none" "message")
	 (jong-project-sub-default-dir-3 . "/home/jongyoungcha/go/src/swit/swit-gcs-file-golang/")
	 (jong-project-sub-default-dir-2 . "/home/jongyoungcha/go/src/swit/swit-gcs-file-golang/")
	 (jong-project-sub-default-dir-3 . "/Users/swit-mac/go/src/swit/swit-gcs-file-golang/")
	 (jong-project-sub-default-dir-2 . "/Users/swit-mac/go/src/swit/swit-gcs-file-golang/")
	 (jong-project-sub-default-dir-3 . "/Users/swit-mac/go/src/swit/swit-grpc-activity-golang/")
	 (jong-project-sub-default-dir-2 . "/Users/swit-mac/go/src/swit/swit-grpc-activity-golang/")
	 (jong-project-sub-default-dir-3 . "/Users/swit-mac/go/src/swit/swit-apiV1/")
	 (jong-project-sub-default-dir-2 . "/Users/swit-mac/go/src/swit/swit-apiV1/")
	 (jong-project-sub-default-dir-3 . "/Users/swit-mac/go/src/swit/swit-grpc-workspace-golang/")
	 (jong-project-sub-default-dir-2 . "/Users/swit-mac/go/src/swit/swit-grpc-workspace-golang/")
	 (jong-project-sub-default-dir-3 . "/Users/swit-mac/go/src/swit/swit-api-golang/")
	 (jong-project-sub-default-dir-2 . "/Users/swit-mac/go/src/swit/swit-api-golang/")
	 (jong-project-sub-default-dir-3 . "/Users/swit-mac/goworks/src/swit/swit-grpc-asset-golang/")
	 (jong-project-sub-default-dir-2 . "/Users/swit-mac/goworks/src/swit/swit-grpc-asset-golang/")
	 (jong-project-sub-default-dir-3 . "/Users/swit-mac/goworks/src/swit/swit-api-golang/")
	 (jong-project-sub-default-dir-2 . "/Users/swit-mac/goworks/src/swit/swit-api-golang/")
	 (jong-project-sub-default-dir-3 . "/Users/swit-mac/goworks/src/awesomeProject/")
	 (jong-project-sub-default-dir-2 . "/Users/swit-mac/goworks/src/awesomeProject/")
	 (jong-project-sub-default-dir-3 . "/Users/swit-mac/goworks/src/github.com/jongyoungcha/Chanker/")
	 (jong-project-sub-default-dir-2 . "/Users/swit-mac/goworks/src/github.com/jongyoungcha/Chanker/")
	 (jong-project-sub-default-dir-3 . "/home/jongyoungcha/projects/test/")
	 (jong-project-sub-default-dir-2 . "/home/jongyoungcha/projects/test/")
	 (projectile-project-root . "/home/jongyoungcha/projects/cmake-project-template/")
	 (jong-project-sub-default-dir-3 . "/home/jongyoungcha/projects/Ants/")
	 (jong-project-sub-default-dir-2 . "/home/jongyoungcha/projects/Ants/")
	 (projectile-project-root . "/home/jongyoungcha/projects/Ants/")
	 (jong-project-sub-default-dir-3 . "/home/jongyoungcha/goworks/src/github.com/jongyoungcha/test/")
	 (jong-project-sub-default-dir-2 . "/home/jongyoungcha/goworks/src/github.com/jongyoungcha/test/")
	 (jong-project-sub-default-dir-3 . "/home/jongyoungcha/goworks/src/github.com/jongyoungcha/Chanker/")
	 (jong-project-sub-default-dir-2 . "/home/jongyoungcha/goworks/src/github.com/jongyoungcha/Chanker/")
	 (jong-project-sub-default-dir-3 . "/home/jongyoungcha/projects/Chanker/")
	 (jong-project-sub-default-dir-2 . "/home/jongyoungcha/projects/Chanker/")
	 (jong-project-sub-default-dir-3 . "/home/jongyoungcha/.emacs.d/")
	 (jong-project-sub-default-dir-2 . "/home/jongyoungcha/.emacs.d/")
	 (jong-project-sub-default-dir-3 . "/home/jongyoungcha/projects/actor-pattern/")
	 (jong-project-sub-default-dir-2 . "/home/jongyoungcha/projects/actor-pattern/")
	 (jong-project-sub-default-dir-3 . "/home/jongyoungcha/goworks/src/bitbucket.org/spooncast/__meari-server/")
	 (jong-project-sub-default-dir-2 . "/home/jongyoungcha/goworks/src/bitbucket.org/spooncast/__meari-server/")
	 (jong-project-sub-default-dir-3 . "/home/jongyoungcha/goworks/src/bitbucket.org/spooncast/meari-server/")
	 (jong-project-sub-default-dir-2 . "/home/jongyoungcha/goworks/src/bitbucket.org/spooncast/meari-server/")
	 (jong-project-sub-default-dir-3 . "/home/jongyoungcha/goworks/src/bitbucket.org/spooncast/gotest/")
	 (jong-project-sub-default-dir-2 . "/home/jongyoungcha/goworks/src/bitbucket.org/spooncast/gotest/")
	 (jong-project-sub-default-dir-3 . "/home/jongyoungcha/goworks/src/bitbucket.org/meari-server/")
	 (jong-project-sub-default-dir-2 . "/home/jongyoungcha/goworks/src/bitbucket.org/meari-server/")
	 (jong-project-sub-default-dir-3 . "/home/jongyoungcha/projects/rust-projects/rust_test/")
	 (jong-project-sub-default-dir-2 . "/home/jongyoungcha/projects/rust-projects/rust_test/")
	 (projectile-project-root . "/home/jongyoungcha/projects/rust-projects/rust_test/")
	 (jong-project-sub-command-3 . "none")
	 (jong-project-sub-default-dir-3 . "/home/jongyoungcha/goworks/src/github.com/jongyoungcha/meari-server-go/")
	 (jong-project-sub-command-2 . "none")
	 (jong-project-sub-default-dir-2 . "/home/jongyoungcha/goworks/src/github.com/jongyoungcha/meari-server-go/")))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
	 (40 . "#c8805d801780")
	 (60 . "#bec073400bc0")
	 (80 . "#b58900")
	 (100 . "#a5008e550000")
	 (120 . "#9d0091000000")
	 (140 . "#950093aa0000")
	 (160 . "#8d0096550000")
	 (180 . "#859900")
	 (200 . "#66aa9baa32aa")
	 (220 . "#57809d004c00")
	 (240 . "#48559e556555")
	 (260 . "#392a9faa7eaa")
	 (280 . "#2aa198")
	 (300 . "#28669833af33")
	 (320 . "#279993ccbacc")
	 (340 . "#26cc8f66c666")
	 (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))
 '(window-divider-mode nil)
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:background "#444444"))) t)
 '(flymake-error ((((class color)) (:background "#444444"))))
 '(flymake-warning ((((class color)) (:background "#4444aa"))))
 '(flymake-warnline ((((class color)) (:background "#4444aa"))) t)
 '(rtags-errline ((t (:background "IndianRed3" :foreground "white" :underline (:color "white" :style wave)))))
 '(rtags-warnline ((t (:background "royal blue" :foreground "white" :underline (:color "white" :style wave))))))
(put 'dired-find-alternate-file 'disabled nil)
