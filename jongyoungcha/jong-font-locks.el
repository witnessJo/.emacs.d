;;;Code:

(defvar jong-font-locks-term nil "font-lock for term.")
(setq jong-font-locks-term
      '(
        ;; Good cases
        ("[D\\|d]one" . 'font-lock-function-name-face)
        ("[S\\|s]uccess" . 'font-lock-function-name-face)

        ;; Keywords
        ("[C\\|c]reate" . 'font-lock-keyword-face)
        ("[U\\|u]pdate" . 'font-lock-keyword-face)
        ("[F\\|f]rom" . 'font-lock-keyword-face)
        ("[S\\|s]elect" . 'font-lock-keyword-face)
        ("[T\\|t]est" . 'font-lock-keyword-face)
        
        ;; Bad cases
        ("[E\\|e]rror" . 'font-lock-warning-face)
        ("[F\\|f]ail" . 'font-lock-warning-face)
        ("[D\\|d]elete" . 'font-lock-warning-face)
        ("[D\\|d]rop" . 'font-lock-warning-face)
        ))

(font-lock-add-keywords 'vterm-mode jong-font-locks-term)
(font-lock-add-keywords 'eshell-mode jong-font-locks-term)
(font-lock-add-keywords 'term-mode jong-font-locks-term)
(font-lock-add-keywords 'shell-mode jong-font-locks-term)

(provide 'jong-font-locks)

