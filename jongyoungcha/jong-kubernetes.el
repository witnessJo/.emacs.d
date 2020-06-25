
(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))


(use-package kubernetes-tramp
  :ensure t)

(defun jong-kubernetes-pods-exec ()
  (interactive)
  (let ((selected-pod)
		(pod-buffer-name)
		(pod-buffer)
		(rows (split-string (shell-command-to-string "kubectl get pods --no-headers=true | awk '{print $1}'") "\n")))
    (setq selected-pod (helm :sources (helm-build-sync-source "kubernetes-pod"
										:candidates rows
										:fuzzy-match t
										:action (lambda (pod) pod))
							 :buffer "*jong-kubernetes-pods-exec*"))
	
	(setq pod-buffer-name (format "*%s*" selected-pod))
	
	(with-current-buffer (setq pod-buffer (get-buffer-create pod-buffer-name))
	  (display-buffer pod-buffer)
	  (shell-command (format "kubectl exec -it %s /bin/sh" selected-pod) pod-buffer pod-buffer)
	  (shell-mode)
	  )
	)
  )


(provide 'jong-kubernetes)
