
(use-package protobuf-mode
  :ensure t
  :config
  (setq auto-mode-alist  (cons '(".proto$" . protobuf-mode) auto-mode-alist)))

(defconst jong-protobuf-style
  '((c-basic-offset . 2)
	(indent-tabs-mode . nil)))

(add-hook 'protobuf-mode-hook
		  (lambda () (c-add-style "jong-protobuf-style" jong-protobuf-style t)))

(provide 'jong-grpc)
