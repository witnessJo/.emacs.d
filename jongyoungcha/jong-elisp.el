
(use-package elisp-refs
  :ensure t)


(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (define-key emacs-lisp-mode-map (kbd "C-c r .") 'xref-find-definitions)
	    (define-key emacs-lisp-mode-map (kbd "C-c r ,") 'elisp-refs-symbol)
	    ))



(provide 'jong-elisp)
