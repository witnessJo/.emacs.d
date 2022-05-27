
(use-package plantuml-mode
  :ensure t
  :config
  (setq plantuml-jar-path "/Users/richard/Downloads/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-output-type "txt")
  )

;; Sample jar configuration

(provide 'jong-plantuml)
