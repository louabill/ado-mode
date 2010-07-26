;;; Would like to have a command which 
(defun nada ()
  (interactive)
(apply 'append
 (mapcar (function (lambda (dirname) (directory-files dirname nil ".*[.]ado$")))
			  (directory-files "/Universal/Custom/Stata/ado/Downloads" t "^[a-z_0-9]$")))
)

(defun ado-find-ado-dirs (&optional dir subdir)
  (interactive)
  (unless dir
	(setq dir "/Universal/Custom/Stata/ado/Downloads"))
  (unless subdir
	(setq subdir "all"))
  (append 
   (if (or (string= subdir "all") (string= subdir "self"))
	   (list dir))
   (if (or (string= subdir "all") (string= subdir "sub"))
	   (directory-files dir t "^[a-z_0-9]$"))
   nil
   )) 

(defun dammit ()
  "WTF"
  (interactive)
  (let ((dir "/Universal/Custom/Stata/ado/Downloads"))
  (append
   (if (or nil t)
	   (list dir))
   (if (or t nil)
	   (directory-files dir t "^[a-z_0-9]$"))
   ))
)

(defun ado-add-plus ()
  (interactive)
  (ado-modify-font-lock-keywords "/Universal/Custom/Stata/ado/Downloads" 'ado-plus-harmless-face))

(defun ado-add-personal ()
  (interactive)
  (ado-modify-font-lock-keywords "/Universal/Custom/Stata/ado/new" ''ado-personal-harmless-face))

(defun ado-remove-personal ()
  (interactive)
  (ado-modify-font-lock-keywords "/Universal/Custom/Stata/ado/new" ''ado-personal-harmless-face t))


(defun ado-modify-font-lock-keywords (dir face &optional remove subdir extension)
  "Base function used to add or remove keywords for font lock. Gets called
by functions adding keywords for all commands in a directory, as well as the
commands defined in the split directories (a, b, c etc.) that Stata loves so
well. This function is called by wrappers to set up Stata's named directories.
The arguments are
  dir: the directory to look in
  face: the face to use
  remove: if nil, then add the keywords, otherwise remove them
  subdir: subdirectory behavior (defaults to -all-)
            self: just look in dir
            sub:  look in the subdirectories only
            all:  look in dir and subdirectories
  extension: default to 'ado' (there is no real reason for anything else yet)"
  (unless extension
	(setq extension "ado"))
  (unless subdir
	(setq subdir "all"))
  ;; first check if dir is a directory
  (unless (file-directory-p dir)
	(error (concat "directory " dir " does not exist")))

  (let (dothis)
	(if remove
		(setq dothis 'font-lock-remove-keywords)
	  (setq dothis 'font-lock-add-keywords))
;  (message (concat "face is " face))
	(funcall dothis 'ado-mode
;	(font-lock-add-keywords 'ado-mode
			 `((,(regexp-opt 
				  (mapcar (function (lambda (name) (substring-no-properties name nil -4))) 
						  (apply 'append
								 (mapcar (function (lambda (dirname) (directory-files dirname nil ".*[.]ado$")))
										 (ado-find-ado-dirs dir subdir)
										 ))) 
				  'words) 1 ,face))
	  ))
	)

(defun superfoo ()
  (interactive)
	(font-lock-add-keywords 'ado-mode
      `((,(regexp-opt (mapcar (function (lambda (name) (substring-no-properties name nil -4))) 
							  (apply 'append
 (mapcar (function (lambda (dirname) (directory-files dirname nil ".*[.]ado$")))
;		 (append (list "/Universal/Custom/Stata/ado/new")
;				 (directory-files "/Universal/Custom/Stata/ado/new" t "^[a-z_0-9]$")
;				 nil)
		 (ado-find-ado-dirs "/Universal/Custom/Stata/ado/new" "all")
			  ))) 'words) 1 'ado-personal-harmless-face))
	  )
	)

(defun unsuperfoo ()
  (interactive)
	(font-lock-remove-keywords 'ado-mode
      `((,(regexp-opt (mapcar (function (lambda (name) (substring-no-properties name nil -4))) 
							  (apply 'append
 (mapcar (function (lambda (dirname) (directory-files dirname nil ".*[.]ado$")))
;		 (append (list "/Universal/Custom/Stata/ado/new")
;				 (directory-files "/Universal/Custom/Stata/ado/new" t "^[a-z_0-9]$")
;				 nil)
		 (ado-find-ado-dirs "/Universal/Custom/Stata/ado/new" "all")
			  ))) 'words) 1 'ado-personal-harmless-face))
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

(provide 'ado-scratch)