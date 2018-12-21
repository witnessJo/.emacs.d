


;;; code ...!!
(add-hook 'active-mark-hook (lambda ()
                              (message "active mark hook!!")
                              ))

(add-hook 'deactive-mark-hook (lambda ()
                                (message-box"deactive mark hook!!")
                                ))



(provide 'chan-cursor-tracker)
