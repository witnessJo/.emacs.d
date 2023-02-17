;;;Code:

(defvar jong-font-locks-term nil "font-lock for term.")
(setq jong-font-locks-term
      '(
        ;; Good cases
        ("[D\\|d]o" . 'font-lock-function-name-face)
        ("[D\\|d]one" . 'font-lock-function-name-face)
        ("[S\\|s]uccess" . 'font-lock-function-name-face)
        ("[S\\|s]ucceed" . 'font-lock-function-name-face)
        ("[I\\|i]nfo" . 'font-lock-function-name-face)
        ("[L\\|l]evel" . 'font-lock-function-name-face)
        ("LEVEL" . 'font-lock-function-name-face)

        ;; Keywords
        ("[Q\\|q]uery" . 'font-lock-keyword-face)
        ("[C\\|c]reate[d]" . 'font-lock-keyword-face)
        ("[C\\|c]REATE[d]" . 'font-lock-keyword-face)
        ("[U\\|u]pdate[d]" . 'font-lock-keyword-face)
        ("[U\\|u]PDATE[d]" . 'font-lock-keyword-face)
        ("[F\\|f]rom" . 'font-lock-keyword-face)
        ("[F\\|f]ROM" . 'font-lock-keyword-face)
        ("[S\\|s]elect" . 'font-lock-keyword-face)
        ("[S\\|s]ELECT" . 'font-lock-keyword-face)
        ("[T\\|t]est" . 'font-lock-keyword-face)
        ("[T\\|t]EST" . 'font-lock-keyword-face)
        ("[D\\|d]ebug" . 'font-lock-keyword-face)
        ("[D\\|d]EBUG" . 'font-lock-keyword-face)
        
        ;; Bad cases
        ("[E\\|e]rror" . 'font-lock-warning-face)
        ("[F\\|f]ail" . 'font-lock-warning-face)
        ("[F\\|f]ailed" . 'font-lock-warning-face)
        ("[D\\|d]elete[d]" . 'font-lock-warning-face)
        ("[D\\|d]ELETE[d]" . 'font-lock-warning-face)
        ("[D\\|d]rop" . 'font-lock-warning-face)
        ("[D\\|d]ROP" . 'font-lock-warning-face)
        ("[W\\|w]arn" . 'font-lock-warning-face)
        ("[W\\|w]arning" . 'font-lock-warning-face)
        ))

(font-lock-add-keywords 'vterm-mode jong-font-locks-term)
(font-lock-add-keywords 'eshell-mode jong-font-locks-term)
(font-lock-add-keywords 'term-mode jong-font-locks-term)
(font-lock-add-keywords 'shell-mode jong-font-locks-term)
(font-lock-add-keywords 'comint-mode jong-font-locks-term)

(provide 'jong-font-locks)

