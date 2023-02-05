
(use-package plantuml-mode
  :ensure t
  :config
  (setq plantuml-jar-path "/Users/jongyoungcha/jongyoungcha/plantuml-1.2023.1.jar")
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-output-type "txt")
  )

;; Sample jar configuration

(provide 'jong-plantuml)
