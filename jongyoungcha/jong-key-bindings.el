;;; Code

;; (cond
;; ((string-equal system-type "gnu/linux")
;; (progn
;; ))
;; ((string-equal system-type "darwin")
;; (progn
;; (global-set-key (kbd "<C-M-up>") 'windmove-up)
;; (global-set-key (kbd "<C-M-down>") 'windmove-down)
;; (global-set-key (kbd "<C-M-left>") 'windmove-left)
;; (global-set-key (kbd "<C-M-right>") 'windmove-right))))


(global-set-key (kbd "C-x <C-up>") 'windmove-up)

(global-set-key (kbd "C-.") 'winner-undo)
(global-set-key (kbd "C->") 'winner-redo)
(global-set-key (kbd "C--") 'jong-common-delete-above-below-window)

(global-set-key (kbd "C-S-o") 'jong-common-open-line-above)
(global-set-key (kbd "C-o") 'jong-common-open-line-below)

(global-set-key (kbd "C-S-c") 'jong-common-copy-region-or-line)
(global-set-key (kbd "C-y") (lambda ()
							  (interactive)
							  (jong-common-open-line-below)
							  (call-interactively 'yank)))

(global-set-key (kbd "C-M-\\") 'jong-common-auto-indent-buffer)
(global-set-key (kbd "C-x C-p") 'jong-common-prev-buffer)
(global-set-key (kbd "C-x C-n") 'jong-common-next-buffer)


(global-set-key (kbd "M-c") 'jong-common-copy-region-or-line)
(global-set-key (kbd "M-v") (lambda()
							  (interactive)
							  (call-interactively 'yank)))


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


(defvar jong-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
	;; (define-key map (kbd "M-w") (lambda () (interactive) (jong-forward-line -1)))
	;; (define-key map (kbd "C-M-w") (lambda () (interactive) (jong-forward-line -1)))
	;; (define-key map (kbd "M-a") 'backward-char)
	;; (define-key map (kbd "M-s") (lambda () (interactive) (jong-forward-line 1)))
	;; (define-key map (kbd "C-M-s") (lambda () (interactive) (jong-forward-line 1)))
	;; (define-key map (kbd "M-d") 'forward-char)

	(define-key map (kbd "<M-left>") 'backward-word)
	(define-key map (kbd "<M-right>") 'forward-word)

	
	(define-key map (kbd "M-<backspace>") 'jong-common-kill-backward-word)
	(define-key map (kbd "C-<backspace>") 'jong-common-kill-backward-word)
	(define-key map (kbd "C-<delete>") 'jong-common-kill-forward-word)
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
	(define-key map (kbd "<M-S-up>") (lambda () (interactive)
									   (jong-set-mark)
									   (jong-forward-line -1)))
	(define-key map (kbd "<M-S-down>") (lambda () (interactive)
										 (jong-set-mark)
										 (jong-forward-line 1)))
	(define-key map (kbd "<M-S-left>") (lambda () (interactive)
										 (jong-set-mark)
										 (syntax-subword-backward 1)))
	(define-key map (kbd "<M-S-right>") (lambda () (interactive)
										  (jong-set-mark)
										  (syntax-subword-forward 1)))
	(define-key map (kbd "C-M-S-a") (lambda () (interactive)
									  (jong-set-mark)
									  (backward-word)))
	(define-key map (kbd "C-M-S-d") (lambda () (interactive)
									  (jong-set-mark)
									  (forward-word)))
	;; (define-key map (kbd "M-<backspace>") (lambda () (
	;; (progn (call-interactively 'backward-kill-word)
	;; (pop kill-ring))))
	;; (define-key map (kbd "M-<delete>") (lambda () (interactive)
	;; (progn (call-interactively 'forward-hf)
	;; (pop kill-ring))))
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
