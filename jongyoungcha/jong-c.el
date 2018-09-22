;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; c develope environments ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun jyc-copy-init-cpp-project()
  (interactive)
  (dired-copy-file-recursive "~/.emacs.d/jongyoungcha/init_cpp_project/" default-directory nil nil nil 'always))


(defun jong-c-insert-predfine ()
  (interactive)
  (let ((filename buffer-file-name) predefined extension)
    (setq filename (file-name-nondirectory filename))
    (when (not (string-empty-p filename))
      (setq extension (file-name-extension filename))
      (if (or (string= extension "h") (string= extension "hpp"))
          (progn
            (setq predefined (upcase (format "_%s_" (replace-regexp-in-string "\\." "_" filename))))
            (setq predefined (upcase (format "%s" (replace-regexp-in-string "\\-" "_" predefined))))
            (insert (format "#ifndef %s\n" predefined))
            (insert (format "#define %s\n" predefined))
            (insert (format "#endif\n")))
        (message "%s" "The file was not a C header file...")))
    ))


(require 'compile)
(add-hook 'c-mode-common-hook
          (lambda ()
            (unless (file-exists-p "Makefile")
              (set (make-local-variable 'compile-command)
                   ;; emulate make's .c.o implicit pattern rule, but with
                   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                   ;; variables:
                   ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s -c -o %s.o %s %s %s"
                             (or (getenv "CC") "gcc")
                             (file-name-sans-extension file)
                             (or (getenv "CPPFLAGS") "-DDEBUG=9")
                             (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
                             file))))))

(use-package smart-compile
  :ensure t)
(with-eval-after-load 'smart-compile
  (add-hook 'c-mode-common-hook
            (lambda ()
              )))

(use-package rtags
  :ensure t)
(with-eval-after-load 'rtags
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  (global-company-mode)
  (rtags-enable-standard-keybindings)
  (add-hook 'rtags-jump-hook 'evil--jumps-push)
  (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'objc-mode-hook 'rtags-start-process-unless-running)
  (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))

  (define-key rtags-mode-map (kbd "<C-return>") 'rtags-select-other-window)
  )

(use-package company-rtags
  :ensure t)
(with-eval-after-load 'company-rtags
  (eval-after-load 'company
    '(add-to-list
      'company-backends 'company-rtags)))

(use-package cmake-ide
  :ensure t)
(with-eval-after-load 'cmake-ide
  (lambda()
    (cmake-ide-setup)))

(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key (kbd "C-c j p") 'jong-c-insert-predfine)
            (local-set-key (kbd "C-c c c") 'compile)
            (local-set-key (kbd "C-g") 'kill-temporary-buffers)
            (local-set-key (kbd "C-S-g") 'close-compilation-window)
            (local-set-key (kbd "C-c f f") 'ff-find-other-file)
            (defun enable-autoreload-for-dir-locals ()
              (when (and (buffer-file-name)
                         (equal dir-locals-file
                                (file-name-nondirectory (buffer-file-name))))
                (add-hook (make-variable-buffer-local 'after-save-hook)
                          'my-reload-dir-locals-for-all-buffer-in-this-directory)))))

;; Set linux indent style
(defvar c-default-style)
(defvar c-basic-offset)

;; Add flycheck c++ modep
(add-hook 'c++-mode-hook (lambda ()
                           (setq flycheck-gcc-language-standard "c++11")))

(add-hook 'c-mode-common-hook (lambda ()
                                (setq c-default-style "linux")
                                (setq-default indent-tabs-mode nil)
                                (setq-default tab-width 3)
                                (setq c-basic-offset 3)))


(use-package ggtags
  :ensure t)

(eval-after-load "ggtags"
  '(progn
     (define-key ggtags-mode-map (kbd "C-c g .") (lambda ()
                                                   (interactive)
                                                   (evil--jumps-push)
                                                   (call-interactively 'ggtags-find-definition)))
     (define-key ggtags-mode-map (kbd "C-c g ,") (lambda ()
                                                   (interactive)
                                                   (evil--jumps-push)
                                                   (call-interactively 'ggtags-find-reference)))))

(use-package helm-gtags
  :ensure t)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (ggtags-mode 1))))


(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)


;; Set key bindings
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))


(provide 'jong-c)
