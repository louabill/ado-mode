(defun ado-ask-filename ()
  (interactive)
  (read-from-minibuffer "What file? "))

(defun ado-open-command ()
  (interactive)
  (ado-open-file-on-adopath (ado-grab-something 0))
  )

(defun ado-open-any-file ()
  (interactive)
  (ado-open-file-on-adopath (ado-ask-filename)))

(defun ado-open-file-on-adopath (filename &optional tmpbuffer)
  (interactive)
  (unless ado-stata-home
	(error "You need to set ado-stata-home to open files on the adopath"))
  (let ((stataDir (file-name-as-directory ado-stata-home))
		(currentDir (file-name-as-directory (expand-file-name ".")))
		theStata fullStata theFile tmpDir tmpLog)
	(unless tmpbuffer
	  (setq tmpbuffer " *stata log*")) ;; space needed to stop undo storage
	;; check for extension
	(unless (file-name-extension filename)
	  (setq filename (concat filename ".ado")))
	;; figure out which stata to run
	(cond 
	 ((string= system-type "darwin")
	  (setq theStata
		  (if (file-directory-p (concat stataDir "Stata.app")) "Stata"
			(if (file-directory-p (concat stataDir "StataSE.app")) "StataSE"
			  (if (file-directory-p (concat stataDir "StataMP.app")) "StataMP"
					  (error "Could not find Stata")))))
	;; because lots of irritating single parens bother me
	  (setq fullStata 
			(concat 
			 (file-name-as-directory
			  (concat 
			   (file-name-as-directory
				(concat 
				 (file-name-as-directory 
				  (concat stataDir theStata ".app")) 
				 "Contents"))
			   "MacOS"))
			 theStata))
	  (setq tmpDir 
			(ado-strip-after-newline 
			 (shell-command-to-string "getconf DARWIN_USER_TEMP_DIR")))
	 	;; (message "%s" (concat "The full stata path is -->" fullStata "<--"))
	  (shell-command (concat "cd " tmpDir " ; " fullStata " -q -b -e  findfile " filename))
	)
	 ((string= system-type "windows-nt")
	  (setq theStata
		  (if (file-exists-p (concat stataDir "Stata.exe")) "Stata"
			(if (file-exists-p (concat stataDir "StataSE.exe")) "StataSE"
			  (if (file-exists-p (concat stataDir "StataMP.exe")) "StataMP"
					  (error "Could not find Stata")))))
	  (setq tmpDir (getenv "TEMP"))
	  (shell-command (concat "cd %TEMP% & \"" stataDir theStata ".exe\" /q /e findfile " filename))
	  ; (message "%s" (concat "\"" stataDir theStata ".exe\" /q /e findfile " filename)))
	  )
	 (t (error "Nothing for unix yet")))
	(setq tmpLog (concat (file-name-as-directory tmpDir) "stata.log"))
	(save-excursion
	  (set-buffer (get-buffer-create tmpbuffer))
	  (insert-file-contents tmpDir nil nil nil t)
	  (if (string= system-type "windows-nt")
		  (progn
			(goto-char (point-min))
			(while (search-forward "\\" nil t)
			  (replace-match "/"))
			))
	  (goto-char (point-max))
	  (forward-line -1)
	  (if (search-forward filename (point-at-eol) t)
		  (setq theFile (ado-strip-after-newline (thing-at-point 'line))))
	  )
	(kill-buffer tmpbuffer)
	; (delete-file tmpLog) ;; left hanging around for checking
	(unless theFile
	  (cd currentDir)
	  (error (concat "File " filename " not found on adopath"))
	  )
	(if ado-open-read-only-flag
		(find-file-read-only theFile)
	  (find-file theFile))
	))

(defun ado-strip-after-newline (theString)
  (interactive)
  (if (string-match "\n.*" theString) 
	  (replace-match "" nil nil theString)
	theString)
  )

(provide 'ado-scratch)