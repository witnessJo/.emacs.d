;;; Code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rust develope environments ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rust-mode
  :ensure t)

(use-package racer
  :ensure t)

(use-package cargo
  :ensure t)

(require 'racer)
(require 'cargo)

(setq rust-format-on-save t)

(provide 'jong-rust)
