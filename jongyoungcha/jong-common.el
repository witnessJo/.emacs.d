;;; Code


(defun jong-common-find-file-other-window()
  (interactive)
  (let ((buffer-source (current-buffer))
        (buffer-target))
    (call-interactively 'helm-find-files)
    (setq buffer-target (current-buffer))
    (unless (eq buffer-source buffer-target)
      (progn
        (pop-to-buffer buffer-target 'other-window)
        (other-window -1)
        (pop-to-buffer-same-window buffer-source)
        )))
  )




(global-set-key (kbd "C-c C-f") 'jong-common-find-file-other-window)
 


(provide 'jong-common)

