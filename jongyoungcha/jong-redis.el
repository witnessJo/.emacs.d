
(use-package eredis
  :ensure t)

(defun jong-redis-connect ()
  (interactive)
  (setq p1 (eredis-connect "localhost" 6379))
  )

(provide 'jong-redis)
