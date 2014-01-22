;;; ado-stata-info --- Passing code to a running Stata from emacs
;; Copyright (c) 2011-2014
;; Bill Rising
;;
;; Author:   Bill Rising
;; Maintainer: Same <brising@mac.com>
;;             URL: http://homepage.mac.com/brising
;; Keywords: ado-mode
;; Version:  0.1.3 of Oct 22, 2013

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
	   ((file-exists-p (concat stataDir "Stata.exe")) (concat stataDir "Stata"))
	   ((file-exists-p (concat stataDir "StataSE.exe")) (concat stataDir "StataSE.exe"))
	   ((file-exists-p (concat stataDir "StataMP.exe")) (concat stataDir "StataMP.exe"))
	   (t (error (concat "Could not find any Stata in " lookhere))))
	  )
	 (t (error "Nothing for unix yet")))
	))


(defun ado-show-stata ()
  (interactive)
  (message (concat "Found: " (ado-find-stata)))
  )

(defun ado-show-tmp-dir ()
  (interactive)
  (message (concat "Found: " (ado-system-tmp-dir)))
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

(defun ado-get-filename-from-stata (theCommand theArgs)
  (interactive)
  (let ((tmpBuffer " *stata log*")
		theFile tmpLog)
	(cond 
	 ((string= system-type "darwin")
	  (shell-command 
	   (concat "cd " (ado-system-tmp-dir) " ; " 
			   (ado-find-stata) " -q -b -e '" theCommand "' '" theArgs "'"))
	  )
	 ((string= system-type "windows-nt")
	  (shell-command 
	   (concat "cd " (ado-system-tmp-dir) " & \"" 
			   (ado-find-stata) "\" /q /e  " theCommand " \"" theArgs "\""))
	  )
	 (t (error "Nothing for unix yet")))
	(setq tmpLog (concat (ado-system-tmp-dir) "stata.log"))
	;; visit tmp directory and manipulate the log
	(save-excursion
	  (set-buffer (get-buffer-create tmpBuffer))
	  (insert-file-contents tmpLog nil nil nil t)
	  ;; need to get rid of nasty \'s from windows paths
	  (if (string= system-type "windows-nt")
		  (progn
			(goto-char (point-min))
			(while (search-forward "\\" nil t)
			  (replace-match "/"))
			))
	  (goto-char (point-max))
	  (forward-line -1)
	  (unless (search-forward "r(" (point-at-eol) t)
		  (setq theFile (ado-strip-after-newline (thing-at-point 'line))))
	  )
	(kill-buffer tmpBuffer)
   ; (delete-file tmpLog) ;; left hanging around for checking
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
