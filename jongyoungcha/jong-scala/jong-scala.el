

(gnutls-available-p)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
			 '("marmalade" . "https://marmalade-repo.org/packages/"))



(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(use-package ensime
  :ensure t)

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(message-box "testmessage")

(provide 'jong-scala)


