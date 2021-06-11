;;; Code


(global-set-key (kbd "C-.") 'winner-undo)
(global-set-key (kbd "C->") 'winner-redo)
(global-set-key (kbd "C--") 'jong-common-delete-above-below-window)

(global-set-key (kbd "C-S-o") 'jong-common-open-line-above)
(global-set-key (kbd "C-o") 'jong-common-open-line-below)

(global-set-key (kbd "C-S-c") 'jong-common-copy-region-or-line)
;; (global-set-key (kbd "C-y") (lambda ()
;; (interactive)
;; (jong-common-open-line-below)
;; (call-interactively 'yank)))

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

;; remapping about the keybinding.
;; (define-key key-translation-map (kbd "<left>") (kbd "C-b"))
;; (define-key key-translation-map (kbd "<right>") (kbd "C-f"))
;; (define-key key-translation-map (kbd "<up>") (kbd "C-p"))
;; (define-key key-translation-map (kbd "<down>") (kbd "C-n"))
;; (define-key key-translation-map (kbd "C-<left>") (kbd "M-b"))
;; (define-key key-translation-map (kbd "C-<right>") (kbd "M-f"))
;; (define-key key-translation-map (kbd "C-S-<left>") (kbd "M-B"))
;; (define-key key-translation-map (kbd "C-S-<right>") (kbd "M-F"))
;; (define-key key-translation-map (kbd "C-S-<right>") (kbd "M-F"))

;; (defun my-backward-kill-word ()
;; (interactive "*")
;; (let ((orig (point)))
;; (skip-syntax-backward "\sw")
;; (delete-region (point) orig)))

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
	(define-key map (kbd "C-M-'") 'toggle-input-method)
	(define-key map (kbd "<A-backspace>") 'evil-delete-backward-word)
	(define-key map (kbd "<A-kp-delete>") 'kill-word)
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
										 (jong-hkforward-line 1)))
	(define-key map (kbd "<A-left>") 'backward-word)
	(define-key map (kbd "<A-right>") 'forward-word)
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
