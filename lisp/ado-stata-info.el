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
  (let ((stata-dir (file-name-as-directory lookhere))
		stata-flavor)
	;; (message (concat "ado-find-stata found a home: " lookhere))
	(cond
	 ((string= system-type "darwin")
	  (setq stata-flavor
			(cond
			 ((file-directory-p (concat stata-dir "Stata.app")) "Stata")
			 ((file-directory-p (concat stata-dir "StataSE.app")) "StataSE")
			 ((file-directory-p (concat stata-dir "StataMP.app")) "StataMP")
			 (t (error (concat "Could not find any Stata in " lookhere)))))
	  ;; because lots of irritating single parens bother me
	  (concat
	   (file-name-as-directory
		(concat
		 (file-name-as-directory
		  (concat
		   (file-name-as-directory
			(concat stata-dir stata-flavor ".app"))
		   "Contents"))
		 "MacOS"))
	   stata-flavor))
	 ((string= system-type "windows-nt")
	  (cond
	   ((file-exists-p (concat stata-dir "Stata-64.exe")) (concat stata-dir "Stata-64.exe"))
	   ((file-exists-p (concat stata-dir "StataSE-64.exe")) (concat stata-dir "StataSE-64.exe"))
	   ((file-exists-p (concat stata-dir "StataMP-64.exe")) (concat stata-dir "StataMP-64.exe"))
	   (t (error (concat "Could not find any Stata in " lookhere)))))
	 ((string= system-type "gnu/linux")
	  (cond
	   ((file-exists-p (concat stata-dir "stata")) (concat stata-dir "stata"))
	   ((file-exists-p (concat stata-dir "stata-se")) (concat stata-dir "stata-se"))
	   ((file-exists-p (concat stata-dir "stata-mp")) (concat stata-dir "stata-mp"))
	   (t (error (concat "Could not find Console Stata (needed for background tasks) in " lookhere)))))
	 (t (error (concat "Nothing for " system-type " yet"))))))

;; if Stata cannot be found, this defaults to "version !!??"
;; not a great idea to use this for the version because the point of
;;   ado-mode is to highlight for a particular version...
(defun ado-get-stata-version ()
  "Return the current version of Stata."
  (interactive)
  (let (the-version)
	  (setq the-version (ado-get-one-result "version"))
	(if the-version
		the-version
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

(defun ado-get-one-result (stata-command &optional stata-command-args)
  "Get the result of STATA-COMMAND fed to Stata.
The optional STATA-COMMAND-ARGS allows feeding an argument to the command.

Needed for getting bits of information about Stata from Stata."
  ;; doesn't work if the result is wrapped; should fix
  (interactive)
  (let ((tmp-log-buffer " *stata log*")
		stata-result stata-log)
	(cond
	 ((string= system-type "darwin")
	  ;; (shell-command
	  ;;  (concat "cd " (shell-quote-argument (ado-system-tmp-dir)) " ; "
	  ;; 		   (shell-quote-argument (ado-find-stata))
	  ;; 		   " -q -b -e '" stata-command "'"
	  ;; 		   (if stata-command-args (concat " '" stata-command-args "'")))))
	  (shell-quote-argument
	   (concat "cd " (ado-system-tmp-dir) " ; "
			   (ado-find-stata)
			   " -q -b -e '" stata-command
			   (if stata-command-args (concat " " stata-command-args )))))
	 ;; ((string= system-type "windows-nt")
	 ;;  (shell-command
	 ;;   (concat "cd " (shell-quote-argument (ado-system-tmp-dir)) " & "
	 ;; 		   (shell-quote-argument (ado-find-stata))
	 ;; 		   " /q /e  " stata-command
	 ;; 		   (if stata-command-args (concat " \"" stata-command-args "\"")))))
	 ((string= system-type "windows-nt")
	  (shell-quote-argument
	   (concat "cd " (ado-system-tmp-dir) " & "
			   (ado-find-stata)
			   " /q /e " stata-command
			   (if stata-command-args (concat " " stata-command-args )))))
	 ((string= system-type "gnu/linux")
	  (shell-quote-argument
	   (concat "cd " (ado-system-tmp-dir) " ; "
			   (ado-find-stata)
			   " -q -e '" stata-command "'"
			   (if stata-command-args (concat " " stata-command-args )))))
	 (t (error (concat "Nothing for " system-type " yet"))))
	(setq stata-log (concat (ado-system-tmp-dir) "stata.log"))
	;; visit tmp directory and manipulate the log
	(with-current-buffer (get-buffer-create tmp-log-buffer)
	  (insert-file-contents stata-log nil nil nil t)
	  (goto-char (point-max))
	  (forward-line -1)
	  (unless (search-forward "r(" (point-at-eol) t)
		  (setq stata-result (ado-strip-after-newline (thing-at-point 'line)))))
	stata-result))

(defun ado-get-filename-from-stata (stata-command stata-command-args)
  "Get the filename for STATA-COMMAND.
Optional argument STATA-COMMAND-ARGS for passing additional arguments to the
Stata command.

Really kind of a dumb design, because the arguments could well just be
part of the command."
  (interactive)
  ;; need to get rid of nasty \'s from windows paths
  (let ((the-file-name (ado-get-one-result stata-command stata-command-args)))
	(if (string= system-type "windows-nt")
		(replace-regexp-in-string "\\\\" "/" the-file-name))
	the-file-name))

;; for finding lists of directories where Stata has files
(defun ado-find-ado-dirs (dir &optional subdir)
  "Find directories where Stata stores files.
DIR is the directory to look in.
The optional second argument SUBDIR gives the subdirectories to look in.
Allowable values are
   all - look in the directory and all single-letter-or-digit subdirectories
   sub - look just in the single-letter-or-digit subdirectories
  self - look just in the given directory
The strange single-letter-or-digit subdirectories come from Stata storing
both its own and downloaded files in such directories. A remnant of old file
systems with 255-file limits."
  
  (interactive)
  (unless subdir
	(setq subdir "all"))
  (append
   (if (or (string= subdir "all") (string= subdir "self"))
	   (list dir))
   (if (or (string= subdir "all") (string= subdir "sub"))
	   (directory-files dir t "^[a-z_0-9]$"))
   nil))

(defun ado-open-file-on-adopath (filename)
  "Open a file on Stata's adopath.
The optional FILENAME argument allows specifying a file name."
  (interactive)
  (unless ado-stata-home
	(error "You need to set ado-stata-home to open files on the adopath"))
  (let (the-file-name)
	(unless (file-name-extension filename)
	  (setq filename (concat filename ".ado")))
	(setq the-file-name (ado-get-filename-from-stata "findfile" filename))
	(unless the-file-name
	  (error (concat "File " filename " not found on adopath")))
	(if ado-open-read-only-flag
		(find-file-read-only the-file-name)
	  (find-file the-file-name))))

(defun ado-strip-after-newline (string-to-fix)
  "Take a string and return everything before a newline.
STRING-TO-FIX is, well, the string to be fixed."
  (interactive)
  (if (string-match "\n.*" string-to-fix)
	  (replace-match "" nil nil string-to-fix)
	string-to-fix))


(provide 'ado-stata-info)
;;; ado-stata-info.el ends here
