
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


(use-package kubectl
  ;; :straight (:host github :repo "jypma/kubectl")
  :if (executable-find "kubectl")
  :config
  (setq kubectl--kubectl (executable-find "kubectl"))
  (defun kubectl-choose-context (context)
    "Select a new context interactively"
    (interactive (list (completing-read "Context: " (kubectl--context-names) nil t)))
    (setq kubectl--context context)
    (make-process :name "kubectx" :command (list (executable-find "kubectx") context))
    (call-interactively 'kubectl-choose-namespace))
  (defun kubectl-choose-namespace (namespace)
    "Select a new namespace interactively"
    (interactive (list (completing-read "Namespace: " (kubectl--namespace-names) nil t)))
    (setq kubectl--namespace namespace)
    (make-process :name "kubens" :command (list (executable-find "kubens") namespace))
    (when kubectl-pods-mode
      (call-interactively #'kubectl-pods-refresh)))
  (general-def kubectl-log-mode-map
    "C-c k" #'kubectl--log-kill-process)
  (general-def 'normal kubectl-log-mode-map
    "q" #'bury-buffer)
  (general-def 'normal kubectl-pods-mode-map
    "c" #'kubectl-choose-context
    "s" #'kubectl-pods-choose-namespace
    "gr" #'kubectl-pods-refresh
    "gl" #'kubectl--pods-log
    "t" #'kubectl--pods-term
    "r" #'kubectl-pods-run
    "i" #'kubectl--pods-inspect
    "d" #'kubectl--pods-dired)
  (general-def 'normal kubectl-deployments-mode-map
    "c" #'kubectl-choose-context
    "s" #'kubectl-choose-namespace
    "gr" #'kubectl--deployments-refresh
    "o" #'kubectl--deployments-open
    "RET" #'kubectl--deployments-open
    "i" #'kubectl--deployments-inspect)
  (general-def 'normal kubectl-statefulsets-mode-map
    "c" #'kubectl-choose-context
    "s" #'kubectl-choose-namespace
    "gr" #'kubectl--statefulsets-refresh
    "o" #'kubectl--statefulsets-open
    "RET" #'kubectl--statefulsets-open
    "i" #'kubectl--statefulsets-inspect)
  :commands (kubectl-choose-context
             kubectl-choose-namespace
             kubectl-deployments
             kubectl-statefulsets))

(when (executable-find "k9s")
  (defun k9s ()
    (interactive)
    (let ((k9s-buf (vterm "*k9s*")))
      (switch-to-buffer k9s-buf)
      (vterm-send-string (executable-find "k9s"))
      (vterm-send-return))))

;; (provide 'init-kubernetes)

(provide 'jong-kubernetes)
