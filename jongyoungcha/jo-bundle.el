;;;Code

(defun jo/tabline-mode()
  (use-package powerline
    :ensure t)

  (global-tab-line-mode t)
  (setq tab-line-new-button-show nil)  ;; do not show add-new button
  (setq tab-line-close-button-show nil)  ;; do not show close button
  (setq tab-line-separator "")

  (defvar my/tab-height 22)
  (defvar my/tab-left (powerline-wave-right 'tab-line nil my/tab-height))
  (defvar my/tab-right (powerline-wave-left nil 'tab-line my/tab-height))

  ;; (defun my/tab-line-tab-name-buffer (buffer &optional _buffers)
  ;; (powerline-render (list my/tab-left
  ;; (format "%s" (buffer-name buffer))
  ;; my/tab-right)))
  ;; (setq tab-line-tab-name-function #'my/tab-line-tab-name-buffer)
  ;; (require 's)
  ;; (defun my/tab-line-buffer-group (buffer)
  ;; "Use the project.el name for the buffer group"
  ;; (with-current-buffer buffer
  ;; (s-chop-suffix "/" (car (project-roots (project-current))))))

  ;; (defun my/buffer-sort (a b) (string< (buffer-name a) (buffer-name b)))
  ;; (setq tab-line-tabs-buffer-group-sort-function #'my/buffer-sort)
  ;; (setq tab-line-tabs-buffer-group-function #'my/tab-line-buffer-group)
  ;; (setq tab-line-tabs-function #'tab-line-tabs-buffer-groups)

  ;; tab color settings
  (set-face-attribute 'tab-line nil ;; background behind tabs
                      :background "gray40"
                      :foreground "gray60" :distant-foreground "gray50"
                      :height 1.0 :box nil)
  (set-face-attribute 'tab-line-tab nil ;; active tab in another window
                      :inherit 'tab-line
                      :foreground "gray70" :background "gray90" :box nil)
  (set-face-attribute 'tab-line-tab-current nil ;; active tab in current window
                      :background "#b34cb3" :foreground "white" :box nil)
  (set-face-attribute 'tab-line-tab-inactive nil ;; inactive tab
                      :background "gray60" :foreground "black" :box nil)
  (set-face-attribute 'tab-line-highlight nil ;; mouseover
                      :background "white" :foreground 'unspecified)
  )
;; (jo/tabline-mode)

(provide 'jo-bundle)
