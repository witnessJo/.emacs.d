;;; Code

(cond
 ((string-equal system-type "gnu/linux")
	(progn
		(global-set-key (kbd "C-x <C-up>") 'windmove-up)
		(global-set-key (kbd "C-x <C-down>") 'windmove-down)
		(global-set-key (kbd "C-x M-b") 'windmove-left)
		(global-set-key (kbd "C-x M-f") 'windmove-right)))
 ((string-equal system-type "darwin")
	(progn
		(global-set-key (kbd "<C-M-up>") 'windmove-up)
		(global-set-key (kbd "<C-M-down>") 'windmove-down)
		(global-set-key (kbd "<C-M-left>") 'windmove-left)
		(global-set-key (kbd "<C-M-right>") 'windmove-right))))


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
(global-set-key (kbd "M-c b") 'jong-common-show-buffer-other-window)
(global-set-key (kbd "M-c M-b") 'jong-common-show-buffer-other-window)
(global-set-key (kbd "M-c f") 'jong-common-find-file-other-window)
(global-set-key (kbd "M-c M-f") 'jong-common-find-file-other-window)

(global-set-key (kbd "M-c M-p") 'jong-common-ring-goto-prev)
(global-set-key (kbd "M-c p") 'jong-common-ring-goto-prev)
(global-set-key (kbd "M-c M-n") 'jong-common-ring-goto-next)
(global-set-key (kbd "M-c n") 'jong-common-ring-goto-next)
(global-set-key (kbd "M-c r c") 'jong-common-ring-clear)
(global-set-key (kbd "M-c r i") 'jong-common-ring-insert)

(global-set-key (kbd "M-c g e") 'jong-common-go-eshell)
(global-set-key (kbd "M-c g s") 'jong-common-go-shell)

(global-set-key (kbd "C-x C-p") 'jong-common-prev-buffer)
(global-set-key (kbd "C-x C-n") 'jong-common-next-buffer)


(global-set-key (kbd "C-S-k") 'jong-common-delete-line)
(global-set-key (kbd "C-k") 'jong-common-kill-line)


(global-set-key (kbd "C-c s C-b") 'jong-common-split-window-left)
(global-set-key (kbd "C-c s C-f") 'jong-common-split-window-right)
(global-set-key (kbd "C-c s C-p") 'jong-common-split-wppindow-up)
(global-set-key (kbd "C-c s C-n") 'jong-common-split-window-down)


(global-set-key (kbd "C-c m C-b") 'jong-common-merge-window-left)
(global-set-key (kbd "C-c m C-f") 'jong-common-merge-window-right)
(global-set-key (kbd "C-c m C-p") 'jong-common-merge-window-up)
(global-set-key (kbd "C-c m C-n") 'jong-common-merge-window-down)

;;; for cua mode
(global-set-key (kbd "C-S-z") 'undo-tree-redo)
(global-set-key (kbd "M-<up>") (lambda ()
															(interactive)
															(jong-forward-line -20)))

(global-set-key (kbd "M-<down>") (lambda ()
															(interactive)
															(jong-forward-line 20)))

(provide 'jong-key-bindings)
