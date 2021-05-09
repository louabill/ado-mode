;;; ado-clip.el --- for clipboard (pasteboard) manipulation of ado-mode -*- lexical-binding: t; package-lint-main-file: "ado-mode.el"; -*-
;;
;; Copyright (C) 2003--2021 Bill Rising

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

;; This file has a bunch of utilities needed for putting
;; things onto the clipboard.
;; It is a series of helper functions for working with the clipboard

;;; Code:

;; it appears that some distros don't load thingatpt automatically
(require 'thingatpt)
(require 'ado-cons)

(declare-function ado-beginning-of-command "ado-mode.el")
(declare-function ado-end-of-command "ado-mode.el")
(declare-function ado-string-trim "ado-mode.el")
(declare-function ado-delimit-is-semi-p "ado-mode.el")

(defun ado-grab-something (&optional what-code)
  "If a region is selected, return the region.
Otherwise react to WHAT-CODE is (this has never been fully implemeted)
If WHAT-CODE is
  nil, return the word at or before point,
   -2, return the entire buffer,
   -1, return the entire command containing the point
    0, just the main command name (for getting help, for instance)
    1, the command prefix directly before the main command
    2, the prefix of the prefix, etc.
As of yet, only -2, -1, and 0 actually are implemented."
  (interactive)
  (let ((mark-even-if-inactive nil)
		(select-enable-clipboard t))
	(if (and mark-active t
			 (not (= (region-beginning) (region-end))))
		(buffer-substring-no-properties (region-beginning) (region-end))
	  (cond
	   ((not what-code)
		(word-at-point))
	   ((= what-code -2)
		(buffer-substring-no-properties (point-min) (point-max)))
	   ((= what-code -1) ;; grab entire command
		(let ((start-here
			   (save-excursion
				 (ado-beginning-of-command)
				 (point)))
			  (end-here
			   (save-excursion
				 (ado-end-of-command)
				 (point)))
			  (select-enable-clipboard t))
		  (buffer-substring-no-properties start-here end-here)))
	   ((= what-code 0) ;; grab entire command
		(let ((end-here
			   (save-excursion
				 (ado-end-of-command)
				 (point))))
		  (save-excursion
			(ado-beginning-of-command)
			(while (search-forward-regexp ".*:" end-here t))
			(skip-chars-forward " /t")
			(word-at-point))))
	   (t (error "%s" "`ado-grab-something': argument must be nil, 0, -1, or -2"))))))

(defun ado-command-to-clip (&optional use-dofile whole-buffer keep-whitespace)
  "Prepare a region or command to send to Stata.
By default, it grabs the current command unless a region is selected.
It then prepares the code for the Command window in Stata by stripping
out both comments and continuations, as well as fixing semicolons
if -#delimit ;- is on. Finally, leading and trailing whitespace (including
blank lines) gets stripped. The resulting command(s) get passed to Stata.

If USE-DOFILE is neither nil nor \"command\", the comments and continuations
are left in the code, as because the code will be run in a do-file.
If WHOLE-BUFFER it non-nil, the entire buffer gets grabbed.
If KEEP-WHITESPACE is non-nil, all whitespace is left as-is.

The grabbing is done by `ado-grab-something'.
The stripping is done by `ado-strip-comments'.
The semicolon-fixing is done by `ado-convert-semicolons'.
The whitespace trimming is done by `ado-string-trim'."
	(unless use-dofile
	  (setq use-dofile "command"))
	(let ((select-enable-clipboard t)
		  (string-to-fix
		   (if whole-buffer
			   (ado-grab-something -2)
			 (ado-grab-something -1))))
	  (unless keep-whitespace
		(setq string-to-fix (ado-string-trim string-to-fix)))
	  (unless (> (length string-to-fix) 0)
		(if whole-buffer
			(error "%s" "Buffer is empty")
		  (error "%s" "No command found")))
	  (if (string= use-dofile "command")
		  (progn
			(setq string-to-fix (ado-strip-comments string-to-fix))
			(if (ado-delimit-is-semi-p)
				(setq string-to-fix (ado-convert-semicolons string-to-fix))))
		(if (ado-delimit-is-semi-p)
			(setq string-to-fix (concat "#delimit ;
" string-to-fix)))) ;; end testing for command
	  (funcall interprogram-cut-function string-to-fix)))

(defun ado-other-to-clip (&optional where prefix suffix)
  "For putting things like 'search' and 'help' onto the clipboard.
Made to be called from other programs only.

WHERE specifies what to grab (see `ado-grab-something').
PREFIX and SUFFIX are for debugging and are for the message put into
the *Messages* buffer when the command runs."
  (let ((select-enable-clipboard t))
	(if prefix (setq prefix (concat prefix " ")))
	(if suffix (setq suffix (concat " " suffix)))
	(message "%s" (concat prefix (ado-grab-something where) suffix))
	(funcall interprogram-cut-function
			 (concat prefix (ado-grab-something where) suffix))))

(defun ado-help-at-point-to-clip ()
  "Put -help <word-at-point>- on the clipboard/pasteboard.
If a region is selected -help <contents of region> is put on the
clipboard/pasteboard."
  (interactive)
  (ado-other-to-clip nil "help"))

(defun ado-help-command-to-clip ()
  "Put -help <current command>- on the clipboard/pasteboard.
Ignores any prefix command. If a region is selected -help <contents of region>
is put on the clipboard/pasteboard."

  (interactive)
  (ado-other-to-clip 0 "help"))

(defun ado-strip-comments (string-to-fix)
  "Strip out all comments from STRING-TO-FIX line by line.
The types of comments cannot be modularized, because of ordering problems."
  (let ((nesting 0)
		(string-to-return "")
		pare-thru
		string-that-matched)
	;; because the command window doesn't like tabs, turn sequences of spaces and tabs into single spaces
	(setq string-to-fix (replace-regexp-in-string "[ 	]+" " " string-to-fix))
	;; adding the space in case // are the last chars on a line
	(while (setq pare-thru (string-match "\\(^///\\|\\( \\|	\\)///\\|^//\\|\\( \\|	\\)//\\|/[*]\\|[*]/\\)" string-to-fix))
	  (setq string-that-matched (match-string 1 string-to-fix))
	  (if (string= "*/" string-that-matched)
		  (error "%s" "Too many */ in a /* */-style comment"))
	  ;; found something to investigate
	  ;; put upto match onto the return string
	  (setq string-to-return
			(concat string-to-return (substring string-to-fix 0 pare-thru)))
	  (setq string-to-fix
			(substring string-to-fix (+ pare-thru (length string-that-matched))))
	  (cond
	   ((or (string= string-that-matched "///")
			(string= string-that-matched " ///")
			(string= string-that-matched "	///"))
		(if (setq pare-thru (string-match "
" string-to-fix))
			(setq string-to-fix (substring string-to-fix (1+ pare-thru)))
		  (error "%s" "Found /// with no continuation")))
	   ((or (string= string-that-matched "//")
			(string= string-that-matched " //")
			(string= string-that-matched "	//"))
		(if (setq pare-thru (string-match "
" string-to-fix))
			(setq string-to-fix (substring string-to-fix pare-thru))
		  (setq string-to-fix "")))
	   (t
		(setq nesting 1)
		(while (> nesting 0)
		  (if (not (setq pare-thru
						 (string-match "\\(/[*]\\|[*]/\\)" string-to-fix)))
			  (error "%s" "Too many /* in a /* */-style comment"))
		  (if (string= (match-string 0 string-to-fix) "*/")
			  (setq nesting (1- nesting))
			(setq nesting (1+ nesting)))
		  ;; ugh too many stacked parens
		  (setq string-to-fix (substring string-to-fix (+ 2 pare-thru)))))))
	(setq string-to-return (concat string-to-return string-to-fix))
	string-to-return))

(defun ado-convert-semicolons (string-to-fix)
  "Fixes semicolons in STRING-TO-FIX.
Converts semicolons to newlines, and combines lines without semicolons."
  (replace-regexp-in-string ";" "\n"
		(mapconcat 'identity (split-string string-to-fix "\n") " ")))

(defun ado-one-eol (string-to-fix)
  "Ensures STRING-TO-FIX ends in an eol.
If it does not, one is appended. The resulting string gets returned."
;  (message (concat "ado-one-eol received ->" string-to-fix "<-"))
  (unless (string-match "\n" (substring-no-properties string-to-fix -1))
	(setq string-to-fix (concat string-to-fix "
")))
;  (message (concat "ado-one-eol wants to return ->" string-to-fix "<-"))
  string-to-fix)

(provide 'ado-clip)

;;; ado-clip.el ends here
