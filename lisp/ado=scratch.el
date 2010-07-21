(defvar foobarvar (regexp-opt '("durnit") 'words))


(apply 'append
 (mapcar (function (lambda (dirname) (directory-files dirname nil ".*[.]ado$")))
			  (directory-files "/Universal/Custom/Stata/ado/Downloads" t "^[a-z_0-9]$")))

(defun superfoo ()
  (interactive)
	(font-lock-add-keywords 'ado-mode
      `((,(regexp-opt (mapcar (function (lambda (name) (substring-no-properties name nil -4))) 
							  (apply 'append
									 (mapcar (function (lambda (dirname) (directory-files dirname nil ".*[.]ado$")))
											 (directory-files "/Universal/Custom/Stata/ado/Downloads" t "^[a-z_0-9]$")))) 
					  'words) 1 ado-plus-harmless-face))
	  )
	)

(defun unsuperfoo ()
  (interactive)
	(font-lock-remove-keywords 'ado-mode
      `((,(regexp-opt (mapcar (function (lambda (name) (substring-no-properties name nil -4))) 
							  (apply 'append
 (mapcar (function (lambda (dirname) (directory-files dirname nil ".*[.]ado$")))
			  (directory-files "/Universal/Custom/Stata/ado/Downloads" t "^[a-z_0-9]$")))) 'words) 1 ado-plus-harmless-face))
	  )
	)


(defun foobar ()
  (interactive)
	(font-lock-add-keywords 'ado-mode
      `((,(regexp-opt (mapcar (function (lambda (name) (substring-no-properties name nil -4))) 
							  (directory-files "/Universal/Custom/Stata/ado/new" nil ".*[.]ado$")) 'words) 1 ado-personal-harmless-face))
	  )
	)

(defun unfoobar ()
  (interactive)
	(font-lock-remove-keywords 'ado-mode
      `((,(regexp-opt (mapcar (function (lambda (name) (substring-no-properties name nil -4))) 
							  (directory-files "/Universal/Custom/Stata/ado/new" nil ".*[.]ado$")) 'words) 1 ado-personal-harmless-face))
	  )
	)


(defun unbleen ()
  (interactive)
  (font-lock-remove-keywords 'ado-mode
		`((,(regexp-opt '("jim" "joe") 'words) 1 ado-oldplace-harmless-face))
	 ))

(defun bleen ()
  (interactive)
  (font-lock-add-keywords 'ado-mode
		`((,(regexp-opt '("jim" "joe") 'words) 1 ado-oldplace-harmless-face))
		)
  )


  (font-lock-add-keywords 
   'ado-mode
;   '(("\\<\\(dammit\\)\\>" 1 ado-plus-harmless-face)))) ;; works
;	(regexp-opt '("dammit") 'words) 1 ado-extras-harmless-face)))

(defun defoobar ()
  (interactive)
  (font-lock-remove-keywords
   'ado-mode
   '("durnit")))