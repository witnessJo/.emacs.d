;;; Code

(global-set-key (kbd "C-.") 'winner-undo)
(global-set-key (kbd "C->") 'winner-redo)
(global-set-key (kbd "C--") 'jong-common-delete-above-below-window)

(global-set-key (kbd "C-S-o") 'jong-common-open-line-above)
(global-set-key (kbd "C-o") 'jong-common-open-line-below)
(global-set-key (kbd "C-S-c") 'jong-common-copy-region-or-line)

(global-set-key (kbd "C-M-\\") 'jong-common-auto-indent-buffer)
(global-set-key (kbd "C-x C-p") 'jong-common-prev-buffer)
(global-set-key (kbd "C-x C-n") 'jong-common-next-buffer)

(global-set-key (kbd "C-S-k") 'jong-common-delete-line)
(global-set-key (kbd "C-k") 'jong-common-kill-line)

(global-set-key (kbd "C-c s <left>") 'jong-common-split-window-left)
(global-set-key (kbd "C-c s <right>") 'jong-common-split-window-right)
(global-set-key (kbd "C-c s <up>") 'jong-common-split-window-up)
(global-set-key (kbd "C-c s <down>") 'jong-common-split-window-down)

(global-set-key (kbd "C-c m <left>") 'jong-common-merge-window-left)
(global-set-key (kbd "C-c m <right>") 'jong-common-merge-window-right)
(global-set-key (kbd "C-c m <up>") 'jong-common-merge-window-up)
(global-set-key (kbd "C-c m <down>") 'jong-common-merge-window-down)

(global-set-key (kbd "C-c C-j ") 'windmove-up)
(global-set-key (kbd "C-c C-k") 'windmove-down)
(global-set-key (kbd "C-c C-h") 'windmove-left)
(global-set-key (kbd "C-c C-l") 'windmove-right)


(global-set-key (kbd "C-c <up>") 'jong-project-run-command)
(global-set-key (kbd "C-c <left>") 'jong-project-sub-command-2)
(global-set-key (kbd "C-c <down>") 'jong-project-run-command)
(global-set-key (kbd "C-c <right>") 'jong-project-run-command)

;; dap
(global-set-key (kbd "<f9>") 'dap-breakpoint-toggle)
(global-set-key (kbd "<f7>") 'dap-step-out)
(global-set-key (kbd "<f7>") 'dap-step-in)
(global-set-key (kbd "<f8>") 'dap-next)
(global-set-key (kbd "<f6>") 'dap-continue)
(global-set-key (kbd "<f5>") 'dap-debug)

(global-set-key (kbd "C-c d d") 'jong-dap-debug-toggle-show-ui)
(global-set-key (kbd "C-c d b") 'dap-ui-breakpoints)
(global-set-key (kbd "C-c d s") 'dap-ui-sessions)
(global-set-key (kbd "C-c d l") 'dap-ui-locals)
(global-set-key (kbd "C-c d p") 'dap-debug-last)
(global-set-key (kbd "C-c d o") 'jong-dap-go-to-output-buffer)

(global-set-key (kbd "C-c d <backspace>") 'dap-delete-session)
(global-set-key (kbd "C-c d k") 'dap-ui-sessions-delete-session)
(global-set-key (kbd "C-c d <return>") 'dap-ui-sessions-select)

(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

;; (global-set-key (kbd "C-c c") 'counsel-compile)
(global-set-key (kbd "C-c g g") 'counsel-git)
(global-set-key (kbd "C-c g j") 'counsel-git-grep)
(global-set-key (kbd "C-c g l") 'counsel-git-log)
(global-set-key (kbd "C-c a g") 'helm-do-ag)
;; (global-set-key (kbd "C-c m") 'counsel-linux-app)
(global-set-key (kbd "C-c n") 'counsel-fzf)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c J") 'counsel-file-jump)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;; (global-set-key (kbd "C-c w") 'counsel-wmctrl)

(global-set-key (kbd "C-c p p") 'counsel-projectile-switch-project)
(global-set-key (kbd "C-c p f") 'counsel-projectile-find-file)

;; remapping about the keybinding.

(defun my-backward-kill-word ()
  "Kill words backward my way."
  (interactive)
  (if (bolp)
      (backward-delete-char 1)
    (if (string-match "^\\s-+$" (buffer-substring (point-at-bol) (point)))
        (kill-region (point-at-bol) (point))
      (backward-kill-word 1))))


(defvar jong-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "<A-left>") 'syntax-subword-backward)
	(define-key map (kbd "<A-right>") 'syntax-subword-forward)
	
	(define-key map (kbd "A-<backspace>") 'syntax-subword-backward-kill)
	(define-key map (kbd "<A-kp-delete>") 'syntax-subword-kill)

	(define-key map (kbd "C-<left>") 'backward-word)
	(define-key map (kbd "C-<right>") 'forward-word)

	
	(define-key map (kbd "M-<up>") 'evil-window-up)
	(define-key map (kbd "M-<left>") 'evil-window-left)
	(define-key map (kbd "M-<down>") 'evil-window-down)
	(define-key map (kbd "M-<right>") 'evil-window-right)
	
	(define-key map (kbd "A-<up>") 'backward-paragraph)
	(define-key map (kbd "A-<down>") 'forward-paragraph)
	(define-key map (kbd "C-M-'") 'toggle-input-method)
	(define-key map (kbd "C-a") 'jong-edit-beginning-of-line-text)
	(define-key map (kbd "M-*") 'mc/edit-lines)
	(define-key map (kbd "M-c") 'jong-common-copy-region-or-line)
	(define-key map (kbd "M-v") 'jong-edit-paste-text)
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
