(defun ado-ask-filename ()
  (interactive)
  (read-from-minibuffer "What file? "))

(defun ado-open-command ()
  (interactive)
  (ado-open-file-on-adopath (ado-grab-something 0))
  )

(defun ado-open-anything ()
  (interactive)
  (ado-open-file-on-adopath (ado-ask-filename)))

(defun ado-open-file-on-adopath (filename)
  (interactive)
  (unless ado-stata-home
	(error "You need to set ado-stata-home to open files on the adopath"))
  (let ((stataDir (file-name-as-directory ado-stata-home))
		theStata fullStata)
	;; (message "%s" (concat "Found -->" filename "<--"))
	;; check for extension
	(unless (file-name-extension filename)
	  (setq filename (concat filename ".ado"))) ;; should soften
	;;  (shell-command-to-string "getconf DARWIN_USER_TEMP_DIR")
	;; figure out which stata to run
	(setq theStata
		  (if (file-directory-p (concat stataDir "Stata.app")) "Stata"
			(if (file-directory-p (concat stataDir "StataSE.app")) "StataSE"
			  (if (file-directory-p (concat stataDir "StataMP.app")) "StataMP"
					  (error "Could not find Stata")))))
	;; because lots of irritating single parens bother me
	(setq fullStata (concat (file-name-as-directory (concat stataDir theStata ".app")) "Contents"))
	(setq fullStata (concat (file-name-as-directory fullStata) "MacOS"))
	(setq fullStata (concat (file-name-as-directory fullStata) theStata))
	;; (message "%s" (concat "The full stata path is -->" fullStata "<--"))
	; the following creates a file -stata.log-
	(shell-command (concat fullStata " -q -b -e  findfile " filename))
	))

(provide 'ado-scratch)