;;; Code

(require 'cl)

(use-package dash
  :ensure t
  :config)

(defgroup jong-common nil
  "Setting for `jong-common."
  :link nil)


(defun jong-common-set-window-1-2 (&optional left-buffer right-top-buffer right-bot-buffer)
  (interactive)
  (let ((wind-base)
		(wind-left)
		(wind-right-top)
		(wind-right-bot))
	(delete-other-windows)

	;; Make windows
	(setq wind-base (selected-window))
	(split-window-right)
	(setq wind-left (selected-window))
	(other-window 1)
	(setq wind-right-top (selected-window))
	(split-window-below)
	(other-window 1)
	(setq wind-right-bot (selected-window))

	;; Alloc buffers to windows
	(ignore-errors
	  (when (and (stringp left-buffer) (get-buffer left-buffer))
		(select-window wind-left)
		(switch-to-buffer (get-buffer left-buffer))))

	(ignore-errors
	  (when (and (stringp right-top-buffer) (get-buffer right-top-buffer))
		(select-window wind-right-top)
		(switch-to-buffer (get-buffer right-top-buffer))))

	(ignore-errors
	  (when (and (stringp right-bot-buffer) (get-buffer right-bot-buffer))
		(select-window wind-right-bot)
		(switch-to-buffer (get-buffer right-bot-buffer))))

	(select-window wind-base)
	))


(defun jong-common-set-window-2-1 (&optional left-top-buffer left-bot-buffer right-buffer)
  (interactive)
  (let ((wind-base)
		(wind-left-top)
		(wind-left-bot)
		(wind-right))
	(delete-other-windows)

	;; Make windows
	(setq wind-base (selected-window))

	(split-window-right)
	(split-window-below)
	(setq wind-left-top (selected-window))
	(other-window 1)
	(setq wind-left-bot (selected-window))
	(other-window 1)
	(setq wind-right (selected-window))

	;; Alloc buffers to windows
	(ignore-errors
	  (when (and (stringp left-top-buffer) (get-buffer left-top-buffer))
		(select-window wind-left-top)
		(switch-to-buffer (get-buffer left-top-buffer))))

	(ignore-errors
	  (when (and (stringp left-bot-buffer) (get-buffer left-bot-buffer))
		(select-window wind-left-bot)
		(switch-to-buffer (get-buffer left-bot-buffer))))

	(ignore-errors
	  (when (and (stringp right-buffer) (get-buffer right-buffer))
		(select-window wind-right)
		(switch-to-buffer (get-buffer right-buffer))))

	(select-window wind-base)
	))


(defun jong-common-set-window-4 (&optional buffer-left-top buffer-right-top buffer-left-bot buffer-right-bot)
  (interactive)
  (let ((wind-base)
		(wind-left-top)
		(wind-right-top)
		(wind-left-bot)
		(wind-right-bot))
	(delete-other-windows)

	;;Make Windows
	(setq wind-base (selected-window))

	(setq wind-left-top (selected-window))
	(split-window-below)
	(split-window-right)
	(other-window 1)
	(setq wind-right-top (selected-window))
	(other-window 1)
	(setq wind-left-bot (selected-window))
	(split-window-right)
	(other-window 1)
	(setq wind-right-bot (selected-window))

	;; Alloc buffers to windows
	(ignore-errors
	  (when (and (stringp buffer-left-top) (get-buffer buffer-left-top))
		(select-window wind-left-top)
		(switch-to-buffer (get-buffer buffer-left-top))))

	(ignore-errors
	  (when (and (stringp buffer-right-top) (get-buffer buffer-right-top))
		(select-window wind-right-top)
		(switch-to-buffer (get-buffer buffer-right-top))))

	(ignore-errors
	  (when (and (stringp buffer-left-bot) (get-buffer buffer-left-bot))
		(select-window wind-left-bot)
		(switch-to-buffer (get-buffer buffer-left-bot))))

	(ignore-errors
	  (when (and (stringp buffer-right-bot) (get-buffer buffer-right-bot))
		(select-window wind-right-bot)
		(switch-to-buffer (get-buffer buffer-right-bot))))

	(select-window wind-base)
	)
  )

(defun jong-common-find-buffer-and-move (target-buffer-name)
  "Move to the window having the TARGET-BUFFER-NAME of the buffer."
  (let ((buffer-name))
	(if (stringp target-buffer-name)
		(dolist (target-window (window-list))
		  (when (equal target-buffer-name (buffer-name (window-buffer target-window)))
			(select-window target-window)))
	  )
	)
  )

(defun jong-common-process-exit-code-and-output (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (interactive)
  (with-temp-buffer
	(list (apply 'call-process program nil (current-buffer) nil args)
		  (buffer-string)))
  )

(defun jong-common-find-file-other-window-string (string)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (let ((file-path)
		(target-line)
		(regex-pattern "^\\(.+\\):\\(.+\\)$"))
	(if (and (stringp string) (string-match regex-pattern string))
		(progn
		  (setq file-path (match-string 1 string))
		  (setq target-line (string-to-number (match-string 2 string)))
		  (find-file-other-window file-path)
		  (with-no-warnings (goto-line target-line))
		  (other-window -1))
	  (message "regex buildind failed...")
	  )
	)
  )

(defun jong-common-auto-indent-buffer()
  "Indent the buffer automatic."
  (interactive)
  (let ((prev-pos (point))
		(indent-func nil))
	(set-mark (point-min))
	(goto-char (point-max))
	(if (setq indent-func (lookup-key (current-local-map) (kbd "TAB")))
		(call-interactively indent-func)
	  (call-interactively 'indent-for-tab-command))
	(goto-char prev-pos)
	(deactivate-mark))
  )

(use-package helm-xref
  :ensure t
  :config
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))


(defcustom jong-common-ring-location  0
  "Test."
  :type 'integer)

(defvar jong-common-ring-size 20)
(defcustom jong-common-ring (make-ring jong-common-ring-size)
  "Ring of markers to implement the marker stack."
  :type 'ring)

(defun jong-common-ring-insert ()
  (interactive)
  "."
  (let ((ring jong-common-ring)
		(marker (make-marker))
		(ring-elems))
	(set-marker marker (point) (current-buffer))
	(setq ring-elems (cdr (cdr ring)))
	;; (ring-insert ring marker)
	;; (setq jong-common-ring-location (1- (ring-length ring)))
	;; (goto-char (ring-ref ring jong-common-ring-location))
	(jong-common-ring-remove-from ring jong-common-ring-location)
	(aset ring-elems jong-common-ring-location marker)
	(setq jong-common-ring-location (1+ jong-common-ring-location))
	))


(defun jong-common-ring-remove-from (ring index)
  "Remove the marker instance located in the index. (RING : the target ring, INDEX : location)."
  (let ((ring-elems (cdr (cdr ring))))
	(unless (ring-p ring)
	  (error "The argument ring was not ring"))
	(unless (integerp index)
	  (error "The argument index was not integer"))
	(setq ring-elems (cdr (cdr ring)))
	(catch 'found
	  (dotimes (i (length ring-elems))
		(when (< i index)
		  (setq i index))
		(if (elt ring-elems i)
			(aset ring-elems i nil)
		  (throw 'found nil)
		  ))
	  nil
	  )
	))


(defun jong-common-ring-status ()
  (interactive)
  "."
  (let ((ring jong-common-ring)
		(ring-elems)
		(elem))
	(setq ring-elems (cdr (cdr ring)))
	(dotimes (i (length ring-elems))
	  (setq elem (elt ring-elems i))
	  (when elem
		(print i)
		(print elem))
	  )
	))


(defun jong-common-ring-goto-last-index ()
  (interactive)
  (let ((ring jong-common-ring)
		(marker-target))
	(setq marker-target (ring-ref ring 0))
	(ignore-errors (switch-to-buffer (marker-buffer marker-target)))
	(goto-char marker-target)
	))


(defun jong-common-ring-clear ()
  "."
  (interactive)
  (setq jong-common-ring (make-ring jong-common-ring-size))
  (setq jong-common-ring-location 0)
  )


(defun jong-common-ring-goto-prev()
  "."
  (interactive)
  (let ((ring-elems (cdr (cdr jong-common-ring)))
		(marker-target)
		(target-buffer))
	(if (> jong-common-ring-location 0)
		(progn
		  (setq jong-common-ring-location (1- jong-common-ring-location))
		  (if (and (setq marker-target (elt ring-elems jong-common-ring-location))
				   (setq target-buffer (marker-buffer marker-target)))
			  (progn
				(switch-to-buffer (marker-buffer marker-target))
				(goto-char marker-target)
				(message "Current position %d" jong-common-ring-location))
			(message "The buffer \"%s\" was not existing. (Current position : %d)"
					 (buffer-name target-buffer) jong-common-ring-location)))
	  (message "(Jong Ring) Oldest position."))
	))


(defun jong-common-ring-goto-next ()
  "."
  (interactive)
  (let ((ring-elems (cdr (cdr jong-common-ring)))
		(marker-target)
		(target-buffer))
	(if (< jong-common-ring-location (jong-common-ring-item-count))
		(progn
		  (setq jong-common-ring-location (1+ jong-common-ring-location))
		  (if (and (setq marker-target (elt ring-elems jong-common-ring-location))
				   (setq target-buffer (marker-buffer marker-target)))
			  (progn
				(switch-to-buffer (marker-buffer marker-target))
				(goto-char marker-target)
				(message "Current position %d" jong-common-ring-location))
			(message "The buffer \"%s\" was not existing. (Current position : %d)"
					 (buffer-name target-buffer) jong-common-ring-location)))
	  (message "(Jong Ring) Most recently position."))
	))


(defun jong-common-ring-item-count ()
  "."
  (let ((count 0)
		(ring-elems (cdr (cdr jong-common-ring))))
	(catch 'found
	  (dotimes (i (length ring-elems))
		(unless (elt ring-elems i)
		  (throw 'found nil))
		(setq count (1+ count))))
	count
	))


(defun jong-common-show-buffer-other-window()
  "Display the buffer to the other window."
  (interactive)
  (let ((target-buffer))
	(other-window 1)
	(helm-buffers-list)
	(other-window -1)
	))

(defun jong-common-find-file-other-window()
  "Find the file other windows."
  (interactive)
  (let ((current-path default-directory)
		(other-window-default-path))
	(progn
	  (other-window 1)
	  (setq other-window-default-path default-directory)
	  (setq default-directory current-path)
	  (call-interactively 'helm-find-files)
	  (setq default-directory other-window-default-path)
	  (other-window -1))
	))


(defun jong-common-go-eshell ()
  (interactive)
  (let ((cmd (format "%s %s" "cd" default-directory)))
	(message "eshell path : %s" cmd)
	(call-interactively 'eshell)
	(with-current-buffer "*eshell*"
	  (eshell-interrupt-process)
	  (insert cmd)
	  (eshell-send-input)
	  (pop-to-buffer-same-window "*eshell*"))
	))

(defun jong-common-go-shell ()
  (interactive)
  (let ((cmd (format "%s %s" "cd" default-directory)))
	(message "shell path : %s" cmd)
	(call-interactively 'shell)
	(with-current-buffer "*shell*"
	  (comint-send-string (get-buffer-process (current-buffer)) cmd)
	  (comint-send-input)
	  )
	)
  )

(defun jong-common-go-vterm ()
  (interactive)
  (let ((cmd (format "%s %s" "cd" default-directory)))
	(message "shell path : %s" cmd)
	(call-interactively 'vterm)
	(with-current-buffer "*vterm*"
	  (comint-send-string (get-buffer-process (current-buffer)) cmd)
	  (comint-send-input)
	  )
	)
  )

(defun jong-common-copy-current-dir ()
  "Copy the current directory to the 'kill-ring."
  (interactive)
  (kill-new default-directory))


(defun jong-common-copy-current-path ()
  "Copy the current path to the 'kill-ring."
  (interactive)
  (kill-new buffer-file-name))


(defcustom jong-common-skippable-buffer-patterns '("^.*\*helm.*" "*Buffer List*")
  "Buffer name patterns for skip and kill."
  :type 'list)

(defun jong-common-next-buffer ()
  "Next-buffer that skips certain buffers."
  (interactive)
  (next-buffer)
  (dolist (skippable-buffer-pattern jong-common-skippable-buffer-patterns)
	(when (string-match skippable-buffer-pattern (buffer-name))
	  (message "skip buffer: %s" (buffer-name))
	  (kill-buffer (current-buffer))
	  (jong-next-buffer))
	))

(defun jong-common-prev-buffer ()
  "Prev-buffer that skips certain buffers."
  (interactive)
  (previous-buffer)
  (dolist (skippable-buffer-pattern jong-common-skippable-buffer-patterns)
	(when (string-match skippable-buffer-pattern (buffer-name))
	  (message "skip buffer: %s" (buffer-name))
	  (kill-buffer (current-buffer))
	  (jong-prev-buffer))
	))


(defun jong-common-delete-line (&optional arg)
  (interactive "P")
  (flet ((kill-region (begin end)
					  (delete-region begin end)))
	(kill-line arg))
  (beginning-of-line-text))


(defun jong-common-kill-line (&optional arg)
  (interactive "P")
  (let ((begin-pos (progn (beginning-of-line) (point)))
		(end-pos (progn (end-of-line) (point))))
	(if (string-match-p "^[[:space:]]+$" (thing-at-point 'line t))
		(progn (beginning-of-line)
			   (print "")
			   (setq begin-pos (point)))
	  (progn (beginning-of-line)
			 (re-search-forward "[[:graph:]]")
			 (if (> (point) end-pos)
				 (goto-char begin-pos)
			   (backward-char 1))
			 (print "text")))
	(call-interactively 'kill-line)
	(beginning-of-line-text))
  )

(defun jong-common-kill-forward-word (arg)
  "It would be changed."
  (interactive "p")
  (let ((next-char (buffer-substring (point) (1+ (point)))))
	(if (string-match (regexp-opt-charset '(? ?\t ?\n)) next-char)
		(call-interactively 'hungry-delete-forward)
	  (delete-region
	   (point)
	   (progn
		 (syntax-subword-forward arg)
		 (point)))
	  )
	))


(defun jong-common-kill-backward-word (arg)
  "It would be changed."
  (interactive "p")
  (let ((prev-char (buffer-substring (1- (point)) (point))))
	(if (string-match (regexp-opt-charset '(? ?\t ?\n)) prev-char)
		(call-interactively 'hungry-delete-backward)
	  (delete-region
	   (point)
	   (progn
		 (syntax-subword-backward arg)
		 (point))))
	))

(defun jong-common-open-line-above ()
  "Insert a newline above the current line and put point at beginning."
  (interactive)
  (unless (bolp)
	(beginning-of-line))
  (newline)
  (forward-line -1)
  (call-interactively 'indent-for-tab-command))


(defun jong-common-open-line-below ()
  "Insert a newline below the current line and put point at beginning."
  (interactive)
  (unless (eolp)
	(end-of-line))
  (newline)
  (call-interactively 'indent-for-tab-command))



(defun jong-common-copy-region-or-line (arg)
  "Chan 'copy current line."
  (interactive "p")
  (let ((prev-pos (point))
		(start-pos)
		(end-pos))
	(if (region-active-p)
		(progn
		  (setq start-pos (region-beginning))
		  (setq end-pos (region-end))
		  (copy-region-as-kill start-pos end-pos))
	  (progn
		(setq start-pos (progn (beginning-of-line) (point)))
		(setq end-pos (progn (end-of-line) (point)))
		(kill-ring-save start-pos end-pos)))
	(goto-char prev-pos))
  )


(defun jong-common-delete-above-below-window ()
  (interactive)
  (cond
   ((window-in-direction 'above)
	(windmove-up)
	(delete-window))
   ((window-in-direction 'below)
	(windmove-down)
	(delete-window))
   )
  )


(defun jong-common-reload-dir-locals ()
  "Reload dir locals for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
	(hack-dir-local-variables-non-file-buffer)))


(defun jong-common-reload-dir-locals-all ()
  "For every buffer with the same `default-directory` as the current buffer's reload dir-locals.el."
  (interactive)
  (let ((dir default-directory))
	(dolist (buffer (buffer-list))
	  (with-current-buffer buffer
		(when (equal default-directory dir))
		(jong-common-reload-dir-locals)))))

(defun jong-common-split-window-left ()
  "Split window with direction and switch buffer."
  (interactive)
  (split-window-horizontally)
  (helm-buffers-list))


(defun jong-common-split-window-right ()
  "Split window with direction and switch buffer."
  (interactive)
  (split-window-horizontally)
  (windmove-right)
  (helm-buffers-list))


(defun jong-common-split-window-up ()
  "Split window with direction and switch buffer."
  (interactive)
  (split-window-vertically)
  (helm-buffers-list))


(defun jong-common-split-window-down ()
  "Split window with direction and switch buffer."
  (interactive)
  (split-window-vertically)
  (windmove-down)
  (helm-buffers-list))


(defun jong-common-merge-window-left ()
  "Split window with direction and switch buffer."
  (interactive)
  (windmove-left)
  (delete-window))


(defun jong-common-merge-window-right ()
  "Split window with direction and switch buffer."
  (interactive)
  (windmove-right)
  (delete-window))


(defun jong-common-merge-window-up ()
  "Split window with direction and switch buffer."
  (interactive)
  (windmove-up)
  (delete-window))


(defun jong-common-merge-window-down ()
  "Split window with direction and switch buffer."
  (interactive)
  (windmove-down)
  (delete-window))


(defun jong-common-buffer-window-left ()
  "Split window with direction and switch buffer."
  (interactive)
  (windmove-left)
  (helm-buffers-list))


(defun jong-common-buffer-window-right ()
  "Split window with direction and switch buffer."
  (interactive)
  (windmove-right)
  (helm-buffers-list))


(defun jong-common-buffer-window-up ()
  "Split window with direction and switch buffer."
  (interactive)
  (windmove-up)
  (helm-buffers-list))


(defun jong-common-buffer-window-down ()
  "Split window with direction and switch buffer."
  (interactive)
  (windmove-down)
  (helm-buffers-list))


(defun jong-common-enlarge-window-horizontally-10 ()
  "Enlarge window horizontally."
  (interactive)
  (enlarge-window-horizontally 10))


(defun jong-common-shrink-window-horizontally-10 ()
  "Shrink window horizontally."
  (interactive)
  (shrink-window-horizontally 10))


(defun jong-common-enlarge-window-vertically-10 ()
  "Enlarge window vertically."
  (interactive)
  (shrink-window-horizontally 10))


(defun jong-common-shrink-window-vertically-10 ()
  "Enlarge window vertically."
  (interactive)
  (shrink-window-horizontally 10))



(add-hook 'after-save-hook 'jong-common-reload-dir-locals)


(provide 'jong-common)
;;;jong-common.el
