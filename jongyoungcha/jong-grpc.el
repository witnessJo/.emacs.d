
(use-package protobuf-mode
  :ensure t
  :config
  (setq auto-mode-alist  (cons '(".proto$" . protobuf-mode) auto-mode-alist)))



(provide 'jong-grpc)
