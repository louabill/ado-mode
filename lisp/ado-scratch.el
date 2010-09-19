;; Can add things nicely, but if the directory listing changes, removal
;;   fails. This happens also with simple keywords, so it is not a result
;;   of the directory listings per se. It looks like the keyword removal 
;;   does not expannd the regexp to pull the word, but instad removes regexps
;;   if they match completely...making a mess of long regexps.

;; Need to have a variable which holds the last added keywords, so that all
;; can be removed properly
;; This could work if the added keywords were added by a naming convention...
;;   already have the stata names, just let the user define his/her own names also
;;   to add directories not covered by Stata's names. The lists could then be
;;   saved in variables named after the names
;; ado-add-keywords


(defun ado-add-plus ()
  (interactive)
  (ado-modify-font-lock-keywords (directory-file-name ado-plus-dir) 'ado-plus-harmless-face))

(defun ado-add-personal ()
  (interactive)
  (ado-modify-font-lock-keywords (directory-file-name ado-personal-dir) ''ado-personal-harmless-face))

(defun ado-add-oldplace ()
  (interactive)
  (ado-modify-font-lock-keywords (directory-file-name ado-oldplace-dir) ''ado-oldplace-harmless-face))

(defun ado-add-site ()
  (interactive)
  (ado-modify-font-lock-keywords (directory-file-name ado-site-dir) ''ado-site-harmless-face))

(defun ado-remove-personal ()
  (interactive)
  (ado-modify-font-lock-keywords (directory-file-name ado-personal-dir) ''ado-personal-harmless-face t))


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

;;; Testing simple cases below this line



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
	 )
;  (setq font-lock-defaults '(ado-font-lock-keywords))
;  (font-lock-set-defaults)
;  (ado-mode)
  )

(defun unbleenx ()
  (interactive)
  (font-lock-remove-keywords 'ado-mode
		`((,(regexp-opt '("jim") 'words) 1 ado-oldplace-harmless-face))
	 )
;  (setq font-lock-defaults '(ado-font-lock-keywords))
;  (font-lock-set-defaults)
;  (ado-mode)
  )

(defun bleen ()
  (interactive)
  (font-lock-add-keywords 'ado-mode
		`((,(regexp-opt '("jim" "joe") 'words) 1 ado-oldplace-harmless-face))
		)
;  (setq font-lock-defaults '(ado-font-lock-keywords))
;  (font-lock-set-defaults)
;  (ado-mode)
  )

(provide 'ado-scratch)










