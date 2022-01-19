;;; Code

;;; buffer
(global-set-key (kbd "C-c m") 'ivy-switch-buffer)
(global-set-key (kbd "C-c C-m") 'ivy-switch-buffer)
(global-set-key (kbd "C-c C-n") 'jong-window-display-buffer-right)
(global-set-key (kbd "C-c n") 'jong-window-display-buffer-right)
(global-set-key (kbd "M-p") 'jong-window-scroll-down-right)
(global-set-key (kbd "M-n") 'jong-window-scroll-up-right)
(global-set-key (kbd "C-x C-p") 'jong-common-prev-buffer)
(global-set-key (kbd "C-x C-n") 'jong-common-next-buffer)

(global-set-key (kbd "C-c [") 'jong-buffer-throw-left)
(global-set-key (kbd "C-c ]") 'jong-buffer-throw-right)
;; (global-set-key (kbd "C-c b j") 'jong-buffer-throw-up)
;; (global-set-key (kbd "C-c b k") 'jong-buffer-throw-down)

;;; disable
(global-set-key (kbd "C-x C-c") nil)

(global-set-key (kbd "C-.") 'undo-tree-undo)
(global-set-key (kbd "C-?") 'undo-tree-redo)
(global-set-key (kbd "C--") 'jong-window-delete-above-below-window)
(global-set-key (kbd "C-S--") 'jong-window-merge-vertically-all)

(global-set-key (kbd "C-S-o") 'jong-common-open-line-above)
(global-set-key (kbd "C-o") 'jong-common-open-line-below)
(global-set-key (kbd "C-S-c") 'jong-common-copy-region-or-line)

(global-set-key (kbd "C-k") 'jong-cursor-delete-line)
(global-set-key (kbd "C-S-k") 'jong-cursor-kill-line)

;; windows
(global-set-key (kbd "C-c s <left>") 'jong-common-split-window-left)
(global-set-key (kbd "C-c s <right>") 'jong-common-split-window-right)
(global-set-key (kbd "C-c s <up>") 'jong-common-split-window-up)
(global-set-key (kbd "C-c s <down>") 'jong-common-split-window-down)

(global-set-key (kbd "C-c C-j ") 'windmove-up)
(global-set-key (kbd "C-c C-k") 'windmove-down)
(global-set-key (kbd "C-c C-h") 'windmove-left)
(global-set-key (kbd "C-c C-l") 'windmove-right)

;; cursor
(global-set-key (kbd "C-S-<down>") 'jong-cursor-move-text-down)
(global-set-key (kbd "C-S-<up>") 'jong-cursor-move-text-up)
(global-set-key (kbd "C-S-e") 'jong-cursor-move-eol-region)
(global-set-key (kbd "C-S-a") 'jong-cursor-move-bol-region)
(global-set-key (kbd "C-M-a") 'jong-edit-beginning-of-line-text)
(global-set-key (kbd "C-M-e") 'move-end-of-line)
(global-set-key (kbd "C-M-f") 'forward-list)
(global-set-key (kbd "C-M-S-f") 'backward-list)
(global-set-key (kbd "C-M-n") 'sp-next-sexp)
(global-set-key (kbd "C-M-p") 'sp-previous-sexp)
(global-set-key (kbd "C-M-t") 'sp-select-next-thing)
(global-set-key (kbd "C-M-S-t") 'sp-select-previous-thing-exchange)
(global-set-key (kbd "C-;") 'avy-goto-line)
(global-set-key (kbd "C-'") 'avy-goto-word-1)

;; run
(global-set-key (kbd "C-c <up>") 'jong-project-run-command)
(global-set-key (kbd "C-c <left>") 'jong-project-sub-command-2)
(global-set-key (kbd "C-c <down>") 'jong-project-run-command)
(global-set-key (kbd "C-c <right>") 'jong-project-run-command)

(global-set-key (kbd "C-c C-n") 'flycheck-next-error)
(global-set-key (kbd "C-c C-p") 'flycheck-previous-error)

;; debug
(global-set-key (kbd "<f9>") 'dap-breakpoint-toggle)
(global-set-key (kbd "<f7>") 'dap-step-out)
(global-set-key (kbd "<f7>") 'dap-step-in)
(global-set-key (kbd "<f8>") 'dap-next)
(global-set-key (kbd "<f6>") 'dap-continue)
(global-set-key (kbd "<f5>") 'dap-debug)
(global-set-key (kbd "C-c d d") 'jong-dap-debug-toggle-show-ui)
(global-set-key (kbd "C-<f5>") 'jong-debug-go-debug-current-test)
(global-set-key (kbd "<f12>") 'dap-hydra)
(global-set-key (kbd "M-<f12>") 'jong-debug-setting-toggle-open-file)
(global-set-key (kbd "C-c d d") 'jong-dap-debug-toggle-show-ui)

(global-set-key (kbd "C-d") nil)
(global-set-key (kbd "C-d r") 'dap-debug-restart)
(global-set-key (kbd "C-d p") 'dap-debug-last)
(global-set-key (kbd "C-d s") 'dap-switch-session)
(global-set-key (kbd "C-d f n") 'dap-down-stack-frame)
(global-set-key (kbd "C-d f p") 'dap-up-stack-frame)
(global-set-key (kbd "C-d f l") 'dap-ui-select-stack-frame)
(global-set-key (kbd "C-d o") 'jong-dap-go-to-output-buffer)
(global-set-key (kbd "C-d d") 'dap-disconnect)
(global-set-key (kbd "C-d k") 'dap-ui-sessions-delete-session)
(global-set-key (kbd "C-d <return>") 'dap-ui-sessions-select)

(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; (global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

;; git
(global-set-key (kbd "C-c g g") 'counsel-git)
(global-set-key (kbd "C-c g l") 'counsel-git-checkout)
(global-set-key (kbd "C-c g c") 'magit-commit)
(global-set-key (kbd "C-c g p") 'magit-push)
(global-set-key (kbd "C-c g j c") 'magit-log-current)
(global-set-key (kbd "C-c g j f") 'magit-log-buffer-file)
(global-set-key (kbd "C-c g b") 'magit-blame-addition)

(global-set-key (kbd "C-c a g") 'helm-do-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c J") 'counsel-file-jump)

(global-set-key (kbd "C-c w 3") 'jong-window-split-3-windows-horizontally-evenly)
(global-set-key (kbd "C-c p p") 'counsel-projectile-switch-project)
(global-set-key (kbd "C-c p f") 'counsel-projectile-find-file)

;; profile
(global-set-key (kbd "C-c q s") 'profiler-start)
(global-set-key (kbd "C-c q S-s") 'profiler-stop)
(global-set-key (kbd "C-c q r") 'profiler-report)

(global-set-key (kbd "M-v") 'jong-edit-paste-text)
(global-set-key (kbd "M-C-v") 'jong-edit-paste-text-below)

;;; etc
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)


;; remapping about the keybinding.
(defvar jong-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "<A-left>") 'syntax-subword-backward)
	(define-key map (kbd "<A-right>") 'syntax-subword-forward)
	
	(define-key map (kbd "A-<backspace>") 'jong-cursor-delete-subword-backward)
	(define-key map (kbd "<A-kp-delete>") 'jong-cursor-delete-subword-forward)
	(define-key map (kbd "C-<backspace>") 'jong-cursor-delete-word-backward)
	(define-key map (kbd "C-<delete>") 'jong-cursor-delete-word-forward)
	
	(define-key map (kbd "C-<left>") 'backward-word)
	(define-key map (kbd "C-<right>") 'jong-cursor-forward-word)

	(define-key map (kbd "C-j") 'jong-cursor-newline-align-above)
	
	(define-key map (kbd "M-<up>") 'evil-window-up)
	(define-key map (kbd "M-<left>") 'evil-window-left)
	(define-key map (kbd "M-<down>") 'evil-window-down)
	(define-key map (kbd "M-<right>") 'evil-window-right)

	(define-key map (kbd "M-A-<up>") 'jong-buffer-throw-up)
	(define-key map (kbd "M-A-<left>") 'jong-buffer-throw-left)
	(define-key map (kbd "M-A-<down>") 'jong-buffer-throw-down)
	(define-key map (kbd "M-A-<right>") 'jong-buffer-throw-right)
	
	(define-key map (kbd "A-<up>") 'backward-paragraph)
	(define-key map (kbd "A-<down>") 'forward-paragraph)
	(define-key map (kbd "C-M-'") 'toggle-input-method)
	(define-key map (kbd "C-a") 'jong-edit-beginning-of-line-text)
	(define-key map (kbd "M-*") 'mc/edit-lines)
	(define-key map (kbd "M-c") 'jong-common-copy-region-or-line)
	(global-set-key (kbd "M-v") 'jong-edit-paste-text)
	(global-set-key (kbd "M-C-v") 'jong-edit-paste-text-below)


	(define-key map (kbd "<S-up>") (lambda () (interactive)
									 (jong-set-mark)
									 (jong-forward-line -1)))
	(define-key map (kbd "<S-down>") (lambda () (interactive)
									   (jong-set-mark)
									   (jong-forward-line 1)))
	(define-key map (kbd "<S-left>") (lambda () (interactive)
									   (jong-set-mark)
									   (backward-char 1)))
	(define-key map (kbd "<S-right>") (lambda () (interactive)
										(jong-set-mark)
										(forward-char 1)))
	(define-key map (kbd "<A-S-up>") (lambda () (interactive)
									   (jong-set-mark)
									   (jong-forward-line -1)))
	(define-key map (kbd "<A-S-down>") (lambda () (interactive)
										 (jong-set-mark)
										 (jong-forward-line 1)))
	(define-key map (kbd "<A-S-left>") (lambda () (interactive)
										 (jong-set-mark)
										 (syntax-subword-backward 1)))
	(define-key map (kbd "<A-S-right>") (lambda () (interactive)
										  (jong-set-mark)
										  (syntax-subword-forward 1)))
	(define-key map (kbd "<C-S-left>") (lambda () (interactive)
										 (jong-set-mark)
										 (jong-cursor-backward-word 1)))
	(define-key map (kbd "<C-S-right>") (lambda () (interactive)
										  (jong-set-mark)
										  (jong-cursor-forward-word 1)))

	(define-key map (kbd "C-M-S-a") (lambda () (interactive)
									  (jong-set-mark)
									  (backward-word)))
	(define-key map (kbd "C-M-S-d") (lambda () (interactive)
									  (jong-set-mark)
									  (forward-word)))
	map)
  "Jong-keys-minor-mode keymap.")

(define-minor-mode jong-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " jong-keys")

(defun enable-jong-keys-minor-mode()
  (interactive)
  (jong-keys-minor-mode 1))

(defun disable-jong-keys-minor-mode()
  (interactive)
  (jong-keys-minor-mode 0))


(provide 'jong-key-bindings)
