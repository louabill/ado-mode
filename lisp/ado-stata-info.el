;;; ado-stata-info --- Passing code to a running Stata from emacs
;; Copyright (c) 2011-2016
;; Bill Rising
;;
;; Author:   Bill Rising
;; Maintainer: Same <brising@alum.mit.edu>
;;             URL: http://louabill.org/stata
;; Keywords: ado-mode
;; Version:  0.1.4 of March 23, 2016

;;; a collection of command for gathering info from Stata
;;;  (of course, this does not (yet) mean gathering info from a running Stata)

;;; things for jumping to other commands, help files and the like

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

(defun ado-reset-adopath ()
  "(Re)sets the variables `ado-personal-dir', `ado-plus-dir', `ado-site-dir',
and `ado-oldplace-dir' to their values you would have when starting a Stata
session, i.e. how they would be set when you begin using Stata."
  (interactive)
  (ado-reset-personal-dir)
  (ado-reset-plus-dir)
  (ado-reset-site-dir)
  (ado-reset-oldplace-dir)
)

(defun ado-reset-personal-dir ()
  "Resets the variable `ado-personal-dir' to the initial value of PERSONAL
from a new Stata sesson."
  (interactive)
  (set-variable 'ado-personal-dir (ado-get-filename-from-stata "display" "c(sysdir_personal)"))
  )

(defun ado-reset-plus-dir ()
  "Resets the variable `ado-plus-dir' to the initial value of PLUS
from a new Stata sesson."
  (interactive)
  (set-variable 'ado-plus-dir (ado-get-filename-from-stata "display" "c(sysdir_plus)"))
  )

(defun ado-reset-site-dir ()
  "Resets the variable `ado-site-dir' to the initial value of SITE
from a new Stata sesson."
  (interactive)
  (set-variable 'ado-site-dir (ado-get-filename-from-stata "display" "c(sysdir_site)"))
  )

(defun ado-reset-oldplace-dir ()
  "Resets the variable `ado-oldplace-dir' to the initial value of OLDPLACE
from a new Stata sesson."
  (interactive)
  (set-variable 'ado-oldplace-dir (ado-get-filename-from-stata "display" "c(sysdir_oldplace)"))
  )

(defun ado-find-stata (&optional lookhere)
  (interactive)
  (unless lookhere
	(if ado-stata-home
		(setq lookhere ado-stata-home)
	  (error "You need to set ado-stata-home to open files on the adopath")))
  (let ((stataDir (file-name-as-directory lookhere))
		theStata)
	;; (message (concat "ado-find-stata found a home: " lookhere))
	(cond 
	 ((string= system-type "darwin")
	  (setq theStata
			(cond
			 ((file-directory-p (concat stataDir "Stata.app")) "Stata")
			 ((file-directory-p (concat stataDir "StataSE.app")) "StataSE")
			 ((file-directory-p (concat stataDir "StataMP.app")) "StataMP")
			 (t (error (concat "Could not find any Stata in " lookhere))))
			)
	  ;; because lots of irritating single parens bother me
	  (concat 
	   (file-name-as-directory
		(concat 
		 (file-name-as-directory
		  (concat 
		   (file-name-as-directory 
			(concat stataDir theStata ".app")) 
		   "Contents"))
		 "MacOS"))
	   theStata
	   )
	  )
	 ((string= system-type "windows-nt")
	  (cond
	   ((file-exists-p (concat stataDir "Stata-64.exe")) (concat stataDir "Stata-64"))
	   ((file-exists-p (concat stataDir "StataSE-64.exe")) (concat stataDir "StataSE-64.exe"))
	   ((file-exists-p (concat stataDir "StataMP-64.exe")) (concat stataDir "StataMP-64.exe"))
	   (t (error (concat "Could not find any Stata in " lookhere))))
	  )
	 (t (error "Nothing for unix yet")))
	))

;; if Stata cannot be found, this defaults to "version !!??"
;; not a great idea to use this for the version because the point of
;;   ado-mode is to highlight for a particular version...
(defun ado-get-stata-version ()
  (interactive)
  (let (theVersion)
;	(condition-case nil
	  (setq theVersion (ado-get-one-result "version"))
;	(error nil)
;	)
	(if theVersion
		theVersion
	  "version !!??")
	))

(defun ado-reset-version-command ()
  (set-variable 'ado-version-command (ado-get-stata-version))
  )

(defun ado-show-stata ()
  (interactive)
  (message (concat "Found: " (ado-find-stata)))
  )

(defun ado-show-tmp-dir ()
  (interactive)
  (message (concat "Found: " (ado-system-tmp-dir)))
  )

(defun ado-show-stata-version ()
  (interactive)
  (message (concat "Found: " (ado-get-stata-version)))
  )

(defun ado-system-tmp-dir ()
  "Returns the temporary directory used by the OS for the user.
This is returned as a true directory name using `file-name-as-directory'
so it can be `concat'ted directly with a file name."
  (interactive)
	(cond 
	 ((string= system-type "darwin")
	  (ado-strip-after-newline 
	   (file-name-as-directory (shell-command-to-string "getconf DARWIN_USER_TEMP_DIR"))))
	 ((string= system-type "windows-nt")
	  (file-name-as-directory (getenv "TEMP")))
	 ((string= system-type "gnu/linux")
	  (file-name-as-directory "/tmp"))
	 (t (error "System temp dir not found, somehow"))
	 )
	)

(defun ado-get-one-result (theCommand &optional theArgs)
  ;; doesn't work if the result is wrapped; should fix
  (interactive)
  (let ((tmpBuffer " *stata log*")
		theResult tmpLog)
	(cond 
	 ((string= system-type "darwin")
	  (shell-command 
	   (concat "cd " (ado-system-tmp-dir) " ; " 
			   (ado-find-stata) " -q -b -e '" theCommand "'"
			   (if theArgs (concat " '" theArgs "'"))
	   )))
	 ((string= system-type "windows-nt")
	  (shell-command 
	   (concat "cd " (ado-system-tmp-dir) " & \"" 
			   (ado-find-stata) "\" /q /e  " theCommand
			   (if theArgs (concat " \"" theArgs "\""))
	  )))
	 (t (error "Nothing for unix yet")))
	(setq tmpLog (concat (ado-system-tmp-dir) "stata.log"))
	;; visit tmp directory and manipulate the log
	(with-current-buffer (get-buffer-create tmpBuffer)
	  (insert-file-contents tmpLog nil nil nil t)
	  (goto-char (point-max))
	  (forward-line -1)
	  (unless (search-forward "r(" (point-at-eol) t)
		  (setq theResult (ado-strip-after-newline (thing-at-point 'line))))
	  )
	theResult
	))

(defun ado-get-filename-from-stata (theCommand theArgs)
  (interactive)
  ;; need to get rid of nasty \'s from windows paths
  (let ((theFile (ado-get-one-result theCommand theArgs)))
	(if (string= system-type "windows-nt")
		(replace-regexp-in-string "\\\\" "/" theFile)
	  )
	theFile
	))

(defun ado-open-file-on-adopath (filename)
  (interactive)
  (unless ado-stata-home
	(error "You need to set ado-stata-home to open files on the adopath"))
  (let ((stataDir (file-name-as-directory ado-stata-home))
		(currentDir (file-name-as-directory (expand-file-name ".")))
		theFile tmpDir tmpLog)
	(unless (file-name-extension filename)
	  (setq filename (concat filename ".ado")))
	; (delete-file tmpLog) ;; left hanging around for checking
	(setq theFile (ado-get-filename-from-stata "findfile" filename))
	(unless theFile
	  (error (concat "File " filename " not found on adopath"))
	  )
	(if ado-open-read-only-flag
		(find-file-read-only theFile)
	  (find-file theFile))
	))

(provide 'ado-stata-info)
