;;; Code

(setq lsp-sqls-connections
      '(((driver . "mysql") (dataSourceName . "yyoncho:local@tcp(localhost:3306)/foo"))
		((driver . "postgresql") (dataSourceName . "host=127.0.0.1 port=5432 user=demeter password=PVtMVsH7 dbname=demeter sslmode=enable"))
		((driver . "postgresql") (dataSourceName . "host=127.0.0.1 port=5432 user=postgres password=sentbe1234. dbname=chandra sslmode=disable"))
	    ((driver . "postgresql") (dataSourceName . "host=127.0.0.1 port=5432 user=postgrsql password=local dbname=chandra sslmode=disable"))
		))

(defun jong-sql-install-sqls
	(interactive)
  )

(add-hook 'sql-mode-hook (lambda()
						   (lsp)
						   (local-set-key (kbd "C-<return>") 'lsp-sql-execute-paragraph)
						   ))

(provide 'jong-sql)
