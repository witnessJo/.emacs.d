;;; package ---- test
;;; Commentary: test
;;;

;;; Code:

(defun jong-window-display-buffer-top()
  "Move a cursor to top window, and show window."
  (interactive)
  (let ((current-window))
	(setq current-window (get-buffer-window))
	(call-interactively 'evil-window-up)
	(ivy-switch-buffer)
	(other-window -1)
	)
  )

(defun jong-window-display-buffer-left()
  "Move a cursor to left window, and show window."
  (interactive)
  (let ((current-window))
	(setq current-window (get-buffer-window))
	(call-interactively 'evil-window-left)
	(ivy-switch-buffer)
	(other-window -1)
	)
  )

(defun jong-window-display-buffer-bottom()
  "Move a cursor to bottom window, and show window."
  (interactive)
  (let ((current-window))
	(call-interactively 'evil-window-down)
	(ivy-switch-buffer)
	(other-window -1)
	)
  )

(defun jong-window-display-buffer-right()
  "Move a cursor to right window, and show window."
  (interactive)
  (let ((current-window (get-buffer-window))
		(current-buffer (current-buffer))
		(target-buffer (ivy-switch-buffer)))
	(ignore-errors (call-interactively 'evil-window-right)
				   (switch-to-buffer target-buffer)
				   (select-window current-window)
				   (switch-to-buffer current-buffer))
	)
  )


(defun jong-window-scroll-down-right()
  "Move scroll down right window."
  (interactive)
  (let ((current-window (get-buffer-window)))
	(ignore)
	(ignore-errors (call-interactively 'evil-window-right)
				   (scroll-down-line 10))
	(select-window current-window)
	)
  )

(defun jong-window-scroll-up-right()
  "Move scroll down right window."
  (interactive)
  (let ((current-window (get-buffer-window)))
	(ignore-errors (call-interactively 'evil-window-right)
				   (scroll-up-line 10))
	(select-window current-window)
	)
  )


(defun jong-window-split-3-windows-horizontally-evenly ()
  (interactive)
  (command-execute 'split-window-horizontally)
  (command-execute 'split-window-horizontally)
  (command-execute 'balance-windows)
  )


(provide 'jong-window)
;;; jong-window.el ends here


