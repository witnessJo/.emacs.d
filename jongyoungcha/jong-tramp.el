(require 'tramp)
(setq tramp-default-method "ssh")
(add-to-list 'tramp-remote-path '~/goworks/bin/)


(provide 'jong-tramp)
