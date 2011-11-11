;; this is ado-clip.el
;; Copyright (c) 2009--2011
;; Author:   Bill Rising
;; Maintainer: Same <brising@alum.mit.edu>
;;             URL: http://homepage.mac.com/brising
;; Keywords: ado-mode
;; Version:  0.1 23feb2009

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
Otherwise react to what-code is (this is a mess and should be redone)
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
	  (if what-code
		  (if (= what-code -2)
			  (filter-buffer-substring (point-min) (point-max) nil t)
			(if (= what-code -1) ;; grab entire command
			  (let ((start-here
					 (save-excursion
					   (ado-beginning-of-command)
					   (point)))
					(end-here
					 (save-excursion
					   (ado-end-of-command)
					   (point)))
					(x-select-enable-clipboard t))
				(filter-buffer-substring start-here end-here nil t))
			  ;; what-code is 0
			  (let ((end-here
					 (save-excursion
					   (ado-end-of-command)
					   (point))))
				(save-excursion
				  (ado-beginning-of-command)
				  (while (search-forward-regexp ".*:" end-here t))
				  (skip-chars-forward " /t")
				  (word-at-point)
				  ))))
		;; what-code is nil; 
		(word-at-point))
	  )))

(defun ado-command-to-clip (&optional use-dofile whole-buffer)
  "Grabs either the region, or if there is no region, the
entire Stata command (or buffer if whole-buffer it non-nil), 
then gets it ready to send to Stata. If use-dofile is 
\"command\", it strips out comments and continuations.
The grabbing is done by \\[ado-grab-something], and the stripping
is done by \\[ado-strip-comments]"
	(unless use-dofile
	  (setq use-dofile "command"))
	(let ((x-select-enable-clipboard t)
		  (theString
		   (if whole-buffer
			   (ado-grab-something -2)
			 (ado-grab-something -1)))
		  )
	  (if (string= use-dofile "command")
		  (setq theString (ado-strip-comments theString)))
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
