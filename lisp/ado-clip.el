;; this is ado-clip.el
;; Copyright (c) 2009--2016
;; Author:   Bill Rising
;; Maintainer: Same <brising@alum.mit.edu>
;;             URL: http://louabill.org/stata
;; Keywords: ado-mode
;; Version:  0.5.1   8oct2012-updated for emacs 24 
;; Version:  0.5    11jan2012 (the 0.5 is an arbitrary number)

;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description:
;;
;; This file has a bunch of utilities needed for putting
;; things onto the clipboard.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; It is a series of helper functions for working with the clipboard

;; it appears that some distros don't load thingatpt automatically
(require 'thingatpt)

(defun ado-grab-something (&optional what-code)
  "If a region is selected, return the region.
Otherwise react to what-code is (this is a mess; consider it obsolete)
If what-code is
  nil, return the word at or before point,
   -2, return the entire buffer,
   -1, return the entire command containing the point
    0, just the main command name (for getting help, for instance)
    1, the command prefix directly before the main command
    2, the prefix of the prefix, etc.
As of yet, only -2, -1, and 0 actually are implemented."
  (interactive)
  (let ((mark-even-if-inactive nil)
		(x-select-enable-clipboard t)
		)
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
			  (x-select-enable-clipboard t))
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
			(word-at-point)
			)))
	   (t (error "ado-grab-something: argument must be nil, 0, -1, or -2"))
		)
	  )))

(defun ado-command-to-clip (&optional use-dofile whole-buffer)
  "Grabs either the region, or if there is no region, the
entire Stata command (or buffer if whole-buffer it non-nil), 
then gets it ready to send to Stata. If use-dofile is 
\"command\", it strips out comments and continuations, and spruces
up semicolons if the outdated #delimit ; is in play.
The grabbing is done by \\[ado-grab-something], the stripping
is done by \\[ado-strip-comments], and the semicolon-fixing by
\\[ado-convert-semicolons]."
	(unless use-dofile
	  (setq use-dofile "command"))
	(let ((x-select-enable-clipboard t)
		  (theString
		   (if whole-buffer
			   (ado-grab-something -2)
			 (ado-grab-something -1)))
		  )
	  (unless theString
		(if whole-buffer
			(error "Buffer is empty")
		  (error "No command found")))
	  (if (string= use-dofile "command")
		  (progn
			(setq theString (ado-strip-comments theString))
			(if (ado-delimit-is-semi)
				(setq theString (ado-convert-semicolons theString)))
			)
		(if (ado-delimit-is-semi)
			(setq theString (concat "#delimit ;
" theString)))
		) ;; testing for command
	  (funcall interprogram-cut-function theString)
	))

(defun ado-other-to-clip (&optional where prefix suffix)
  "For putting things like 'search' and 'help' onto the clipboard.
Made to be called from other programs only."
  (let ((x-select-enable-clipboard t))
	(if prefix (setq prefix (concat prefix " ")))
	(if suffix (setq suffix (concat " " suffix)))
	(message (concat prefix (ado-grab-something where) suffix))
	(funcall interprogram-cut-function 										
			 (concat prefix (ado-grab-something where) suffix))
	))

(defun ado-help-at-point-to-clip ()
  "Puts -help <word-at-point>- on the clipboard/pasteboard. If a region is
selected this is what is sent, instead."
  (interactive)
  (ado-other-to-clip nil "help"))

(defun ado-help-command-to-clip ()
  "Puts help for the current command (but not the prefixes) on the 
clipboard/pasteboard. If a region is selected, this is sent, instead."
  (interactive)
  (ado-other-to-clip 0 "help"))

(defun ado-strip-comments (theString)
  "Strips out all comments from a selection line by line.
These cannot be modularized, because of ordering problems"
  (let ((nesting 0) 
		(returnString "") 
		pareThru
		matchString)
	;; because the command window doesn't like tabs, turn sequences of spaces and tabs into single spaces
	(setq theString (replace-regexp-in-string "[ 	]+" " " theString))
	;; adding the space in case // are the last chars on a line
	(while (setq pareThru (string-match "\\(^///\\|\\( \\|	\\)///\\|^//\\|\\( \\|	\\)//\\|/[*]\\|[*]/\\)" theString))
	  (setq matchString (match-string 1 theString))
	  (if (string= "*/" matchString)
		  (error "Too many */ in a /* */-style comment"))
	  ;; found something to investigate
	  ;; put upto match onto the return string
	  (setq returnString (concat returnString (substring theString 0 pareThru)))
	  (setq theString (substring theString (+ pareThru (length matchString))))
	  (cond 
	   ((or (string= matchString "///") (string= matchString " ///") (string= matchString "	///"))
		(if (setq pareThru (string-match "
" theString))
			(setq theString (substring theString (1+ pareThru)))
		  (error "Found /// with no continuation")))
	   ((or (string= matchString "//") (string= matchString " //") (string= matchString "	//"))
		(if (setq pareThru (string-match "
" theString))
			  (setq theString (substring theString pareThru))
		  (setq theString "")))
	   (t 
		(setq nesting 1)
		(while (> nesting 0)
		  (if (not (setq pareThru (string-match "\\(/[*]\\|[*]/\\)" theString)))
			  (error "Too many /* in a /* */-style comment"))
		  (if (string= (match-string 0 theString) "*/")
			  (setq nesting (1- nesting))
			(setq nesting (1+ nesting)))
		  (setq theString (substring theString (+ 2 pareThru)))
		  ) ;; end of nesting while loop
		))  ;; end of cond function
	  )  ;; end of while searching loop
	(setq returnString (concat returnString theString))
	returnString
	))

(defun ado-convert-semicolons (theString)
  "Converts semicolons to newlines, and combines lines withoug semicolons"
  (replace-regexp-in-string ";" "\n" 
		(mapconcat 'identity (split-string theString "\n") " "))
  )
  					   

(defun ado-one-eol (theString)
  "Looks to see if the theString ends in an eol. If it does not,
one is appended. Nothing too complicated."
;  (message (concat "ado-one-eol received ->" theString "<-"))
  (unless (string-match "\n" (substring-no-properties theString -1))
	(setq theString (concat theString "
")))
;  (message (concat "ado-one-eol wants to return ->" theString "<-"))
  theString
  )

(provide 'ado-clip)
