;;; ado-stata-info --- For passing code to a running Stata from emacs -*- lexical-binding: t; package-lint-main-file: "ado-mode.el"; -*-

;; Copyright (C) 2003--2020 Bill Rising

;; Author:   Bill Rising <brising@alum.mit.edu>
;; Keywords: languages, tools
;; Homepage: https://github.com/louabill/ado-mode

;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 3.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.
;; If not, see <https://www.gnu.org/licenses/>

;;; Commentary:

;; A collection of commands for gathering info from Stata via a
;;   background batch file

;;; Code:

;;; things for jumping to other commands, help files and the like

(require 'ado-cus)
(require 'ado-cons)
(require 'ado-clip)

(defun ado-ask-filename ()
  "Utility for user input of a filename.
No checking for existence is done."
  (interactive)
  (read-from-minibuffer "What file? "))

(defun ado-open-command ()
  "Open the ado-file for the current command."
  (interactive)
  (ado-open-file-on-adopath (ado-grab-something 0)))

(defun ado-open-any-file ()
  "Open the ado-file for a user-submitted command name."
  (interactive)
  (ado-open-file-on-adopath (ado-ask-filename)))

(defun ado-reset-adopath ()
  "(Re)set the ado-path related variables.
This resets `ado-personal-dir', `ado-plus-dir', `ado-site-dir',
and `ado-oldplace-dir' to the values they would have when starting your Stata
session, i.e. how they would be set when you begin using Stata.

The emphasis is 'you' because the information is gotten by running a few Stata
sessions in the background and reading the results of the 'sysdir' macros.
Hence, any information in your global profile.do will be used."
  (interactive)
  (ado-reset-personal-dir)
  (ado-reset-plus-dir)
  (ado-reset-site-dir)
  (ado-reset-oldplace-dir))

(defun ado-reset-personal-dir ()
  "Reset the variable `ado-personal-dir' to the starting value of PERSONAL.
The starting value is the value when Stata gets started."
  (interactive)
  (set-variable 'ado-personal-dir (ado-get-filename-from-stata "display" "c(sysdir_personal)")))

(defun ado-reset-plus-dir ()
  "Reset the variable `ado-plus-dir' to the starting value of PLUS.
The starting value is the value when Stata gets started."
  (interactive)
  (set-variable 'ado-plus-dir (ado-get-filename-from-stata "display" "c(sysdir_plus)")))

(defun ado-reset-site-dir ()
  "Reset the variable `ado-site-dir' to the starting value of SITE.
The starting value is the value when Stata gets started."
  (interactive)
  (set-variable 'ado-site-dir (ado-get-filename-from-stata "display" "c(sysdir_site)")))

(defun ado-reset-oldplace-dir ()
  "Reset the variable `ado-oldplace-dir' to the starting value of OLDPLACE.
The starting value is the value when Stata gets started."
  (interactive)
  (set-variable 'ado-oldplace-dir (ado-get-filename-from-stata "display" "c(sysdir_oldplace)")))

(defun ado-find-stata (&optional lookhere)
  "Locate where Stata was installed, if possible. Otherwise ask for help.
Optional LOOKHERE argument allows specifying a non-standard place to look."
  
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
			 (t (error (concat "Could not find any Stata in " lookhere)))))
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
	   theStata))
	 ((string= system-type "windows-nt")
	  (cond
	   ((file-exists-p (concat stataDir "Stata-64.exe")) (concat stataDir "Stata-64.exe"))
	   ((file-exists-p (concat stataDir "StataSE-64.exe")) (concat stataDir "StataSE-64.exe"))
	   ((file-exists-p (concat stataDir "StataMP-64.exe")) (concat stataDir "StataMP-64.exe"))
	   (t (error (concat "Could not find any Stata in " lookhere)))))
	 ((string= system-type "gnu/linux")
	  (cond
	   ((file-exists-p (concat stataDir "stata")) (concat stataDir "stata"))
	   ((file-exists-p (concat stataDir "stata-se")) (concat stataDir "stata-se"))
	   ((file-exists-p (concat stataDir "stata-mp")) (concat stataDir "stata-mp"))
	   (t (error (concat "Could not find Console Stata (needed for background tasks) in " lookhere)))))
	 (t (error (concat "Nothing for " system-type " yet"))))))

;; if Stata cannot be found, this defaults to "version !!??"
;; not a great idea to use this for the version because the point of
;;   ado-mode is to highlight for a particular version...
(defun ado-get-stata-version ()
  "Return the current version of Stata."
  (interactive)
  (let (theVersion)
	  (setq theVersion (ado-get-one-result "version"))
	(if theVersion
		theVersion
	  "version !!??")))

(defun ado-reset-version-command ()
  "Set the variable `ado-version-command' to the current version of Stata."
  (set-variable 'ado-version-command (ado-get-stata-version)))

(defun ado-show-stata ()
  "Show where `ado-mode' thinks Stata is installed."
  (interactive)
  (message "%s" (concat "Found: " (ado-find-stata))))

(defun ado-show-tmp-dir ()
  "Show where Stata's tmpdir is located."
  (interactive)
  (message "%s" (concat "Found: " (ado-system-tmp-dir))))

(defun ado-show-stata-version ()
  "Show the version of Stata."
  (interactive)
  (message "%s" (concat "Found: " (ado-get-stata-version))))

(defun ado-system-tmp-dir ()
  "Return the temporary directory used by the OS for the user.
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
	 (t (error "System temp dir not found, somehow"))))

(defun ado-get-one-result (theCommand &optional theArgs)
  "Get the result of THECOMMAND fed to Stata.
The optional THEARGS argument allows tinkering with Stata's batch-mode
command-line options.

Needed for getting bits of information about Stata from Stata."
  ;; doesn't work if the result is wrapped; should fix
  (interactive)
  (let ((tmpBuffer " *stata log*")
		theResult tmpLog)
	(cond
	 ((string= system-type "darwin")
	  (shell-command
	   (concat "cd " (ado-system-tmp-dir) " ; "
			   (ado-find-stata) " -q -b -e '" theCommand "'"
			   (if theArgs (concat " '" theArgs "'")))))
	 ((string= system-type "windows-nt")
	  (shell-command
	   (concat "cd " (ado-system-tmp-dir) " & \""
			   (ado-find-stata) "\" /q /e  " theCommand
			   (if theArgs (concat " \"" theArgs "\"")))))
	 ((string= system-type "gnu/linux")
	  (shell-command
	   (concat "cd " (ado-system-tmp-dir) " ; "
			   (ado-find-stata) " -q -e '" theCommand "'"
			   (if theArgs (concat " '" theArgs "'")))))
	 (t (error (concat "Nothing for " system-type " yet"))))
	(setq tmpLog (concat (ado-system-tmp-dir) "stata.log"))
	;; visit tmp directory and manipulate the log
	(with-current-buffer (get-buffer-create tmpBuffer)
	  (insert-file-contents tmpLog nil nil nil t)
	  (goto-char (point-max))
	  (forward-line -1)
	  (unless (search-forward "r(" (point-at-eol) t)
		  (setq theResult (ado-strip-after-newline (thing-at-point 'line)))))
	theResult))

(defun ado-get-filename-from-stata (theCommand theArgs)
  "Get the filename for THECOMMAND using command-line options THEARGS."
  (interactive)
  ;; need to get rid of nasty \'s from windows paths
  (let ((theFile (ado-get-one-result theCommand theArgs)))
	(if (string= system-type "windows-nt")
		(replace-regexp-in-string "\\\\" "/" theFile))
	theFile))

(defun ado-open-file-on-adopath (filename)
  "Open a file on Stata's adopath.
The optional FILENAME argument allows specifying a file name."
  (interactive)
  (unless ado-stata-home
	(error "You need to set ado-stata-home to open files on the adopath"))
  (let (theFile)
	(unless (file-name-extension filename)
	  (setq filename (concat filename ".ado")))
	(setq theFile (ado-get-filename-from-stata "findfile" filename))
	(unless theFile
	  (error (concat "File " filename " not found on adopath")))
	(if ado-open-read-only-flag
		(find-file-read-only theFile)
	  (find-file theFile))))

(defun ado-strip-after-newline (string-to-fix)
  "Take a string and return everything before a newline.
STRING-TO-FIX is, well, the string to be fixed."
  (interactive)
  (if (string-match "\n.*" string-to-fix)
	  (replace-match "" nil nil string-to-fix)
	string-to-fix))


(provide 'ado-stata-info)
;;; ado-stata-info.el ends here
