
(use-package elisp-refs
  :ensure t)


(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (define-key emacs-lisp-mode-map (kbd "C-c r .") 'xref-find-definitions)
            (define-key emacs-lisp-mode-map (kbd "C-c r ,") 'elisp-refs-symbol)
            (define-key emacs-lisp-mode-map (kbd "C-M-i") (lambda() (interactive) (scroll-other-window 15)))
            (define-key emacs-lisp-mode-map (kbd "C-M-o") (lambda() (interactive) (scroll-other-window -15)))))



(provide 'jong-elisp)
