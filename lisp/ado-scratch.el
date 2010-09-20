(defun ado-add-plus ()
  "Adds/updates highlighting for all ado files in `ado-plus-dir'.
This includes the a, b, c, ... subdirectories. If ado-files have been
added or removed since the last update the highlighting list will be
updated appropriately."
  (interactive)
  (ado-modify-font-lock-keywords 'plus (directory-file-name ado-plus-dir) ''ado-plus-harmless-face))

(defun ado-add-personal ()
  "Adds/updates highlighting for all ado files in `ado-personal-dir'.
This includes the a, b, c, ... subdirectories. If ado-files have been
added or removed since the last update the highlighting list will be
updated appropriately."
  (interactive)
  (ado-modify-font-lock-keywords 'personal (directory-file-name ado-personal-dir) ''ado-personal-harmless-face))

(defun ado-add-oldplace ()
  "Adds/updates highlighting for all ado files in `ado-oldplace-dir'.
This includes the a, b, c, ... subdirectories. If ado-files have been
added or removed since the last update the highlighting list will be
updated appropriately."
  (interactive)
  (ado-modify-font-lock-keywords 'oldplace (directory-file-name ado-oldplace-dir) ''ado-oldplace-harmless-face))

(defun ado-add-site ()
  "Adds/updates highlighting for all ado files in `ado-site-dir'.
This includes the a, b, c, ... subdirectories. If ado-files have been
added or removed since the last update the highlighting list will be
updated appropriately."
  (interactive)
  (ado-modify-font-lock-keywords 'site (directory-file-name ado-site-dir) ''ado-site-harmless-face))

(defun ado-remove-personal ()
  "Removes highlighting for all ado files in `ado-personal-dir'.
This includes the a, b, c, ... subdirectories. If ado-files have been
added or removed since the last update the highlighting list will be
updated appropriately."
  (interactive)
  (ado-modify-font-lock-keywords 'personal (directory-file-name ado-personal-dir) ''ado-personal-harmless-face t))

(defun ado-remove-plus ()
  "Removes highlighting for all ado files in `ado-plus-dir'.
This includes the a, b, c, ... subdirectories. If ado-files have been
added or removed since the last update the highlighting list will be
updated appropriately."
  (interactive)
  (ado-modify-font-lock-keywords 'plus (directory-file-name ado-plus-dir) ''ado-plus-harmless-face t))

(defun ado-remove-oldplace ()
  "Removes highlighting for all ado files in `ado-oldplace-dir'.
This includes the a, b, c, ... subdirectories. If ado-files have been
added or removed since the last update the highlighting list will be
updated appropriately."
  (interactive)
  (ado-modify-font-lock-keywords 'oldplace (directory-file-name ado-oldplace-dir) ''ado-oldplace-harmless-face t))

(defun ado-remove-site ()
  "Removes highlighting for all ado files in `ado-site-dir'.
This includes the a, b, c, ... subdirectories. If ado-files have been
added or removed since the last update the highlighting list will be
updated appropriately."
  (interactive)
  (ado-modify-font-lock-keywords 'site (directory-file-name ado-site-dir) ''ado-site-harmless-face t))

(defun ado-modify-font-lock-keywords (name dir face &optional remove subdir extension)
  "Base function used to add or remove keywords for font locking. 
Gets called by functions adding keywords for all commands in 
a directory, as well as the commands defined in the split 
directories (a, b, c etc.) that Stata loves so well. This 
function is called by wrappers to set up Stata's named 
directories, so unless you have some extra directories, you 
don't need this function.

The arguments are
  name:   the internal name ado-mode uses for tracking the changes
          this must be be a symbol
  dir:    the directory to look in
  face:   the face to use (must be double ''ed)
  remove: if nil, then add the keywords, otherwise remove them
  subdir: subdirectory behavior (defaults to -all-)
            self: just look in dir
            sub:  look in the subdirectories only
            all:  look in dir and subdirectories
  extension: defaults to 'ado' (there is no real reason for anything else yet)

Here is an example which adds and then removes the directory foo, 
which has an internal name of bar.

  ;; adding the ado-files
  (ado-modify-font-lock-keywords 'bar /Users/jsmith/foo 
     ''ado-mode-personal-harmless-face)
  ;; updating the highlighting uses the same command
  (ado-modify-font-lock-keywords 'bar /Users/jsmith/foo 
     ''ado-mode-personal-harmless-face)
  ;; removing the highlighting
  (ado-modify-font-lock-keywords 'bar /Users/jsmith/foo 
     ''ado-mode-personal-harmless-face t)"
  (unless extension
	(setq extension "ado"))
  (unless subdir
	(setq subdir "all"))
  ;; first check if dir is a directory
  (unless (file-directory-p dir)
	(error (concat "directory " dir " does not exist")))

  ;; now check to see if -name- exists
  (let (newList (oldList (assoc name ado-added-names)))
	(if oldList
		(progn 
		  (font-lock-remove-keywords 'ado-mode (cdr oldList))
		  (setq ado-added-names (assq-delete-all name ado-added-names))))
	(unless remove
	  (setq newList `((,(regexp-opt 
				  (mapcar (function (lambda (name) (substring-no-properties name nil -4))) 
						  (apply 'append
								 (mapcar (function (lambda (dirname) (directory-files dirname nil ".*[.]ado$")))
										 (ado-find-ado-dirs dir subdir)
										 ))) 
				  'words) 1 ,face)))
	  (font-lock-add-keywords 'ado-mode newList)
	  (setq ado-added-names (append ado-added-names `(,(cons name newList))))
	  )
	))

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

;; (let (foo)
;;   (setq foo `(,(cons 'emacs 'baddocs)))
;;   (setq foo (append foo '((trivial . examples))))
;; ;  foo
;;   (cdr (assoc 'emacs foo))
;; )

;; (let (foo)
;;   (setq foo nil)
;;   (setq foo (append foo `(,(cons 'first `((,(regexp-opt '("jim" "joe") 'words) 1 ado-oldplace-harmless-face))))))
;;   (setq foo (append foo `(,(cons 'second `((,(regexp-opt '("harry" "houdini") 'words) 1 ado-oldplace-harmless-face))))))
;; ;  foo
;;   (font-lock-remove-keywords 'ado-mode (cdr (assoc 'first foo)))
;;   (setq foo (assq-delete-all 'first foo))
;;   (setq foo (assq-delete-all 'second foo))
;;   foo
;; )


(provide 'ado-scratch)
