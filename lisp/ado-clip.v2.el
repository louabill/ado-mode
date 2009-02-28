(defun ado-prep-clipboard (&optional use-dofile)
;  (interactive)
  "Copies the proper chunk to the clipboard, where the 
proper chunk is the current region, if any is selecter, or
else it is the command containing the point. If `use-dofile'
is nil, makes the chunk Command-window safe, otherwise it is
a simple copy to the clipboard."
  (let ((mark-even-if-inactive nil)
		(x-select-enable-clipboard t)
		theString)
	(unless use-dofile
	  (setq use-dofile "command"))
	(if (and mark-active t
			 (not (= (region-beginning) (region-end))))
		(setq theString (filter-buffer-substring (region-beginning) (region-end) nil t)) ;; copy region to theString
	  (setq theString (ado-copy-command t)) ;; copy current command b/c there is no region
	  )
	(if (string= use-dofile "command")
		(setq theString (ado-strip-comments theString)))
	(funcall interprogram-cut-function (ado-one-eol theString))))

(defun ado-strip-comments (theString)
  "Make theString usable by the Command window and returns
the result."
  (ado-strip-trip-slash (ado-strip-double-slash (ado-strip-c-comments theString)))
  )

(defun ado-one-eol (theString)
  "Strips trailing whitespace from a string, then adds one eol, 
and returns the result"
  (string-match "[ \t\n]*$" theString (1- (length theString)))
   (while (not (string= "" (match-string 0 theString)))
 	(setq theString (substring theString 0 (match-beginning 0)))
	(string-match "[ \t\n]*$" theString (1- (length theString))))
  (concat theString "
")
  )

(defun ado-strip-trip-slash (theString)
  "Strips /// continuation marks a string, returning the result"
  (while 
	  (string-match "[ 	]*///.*
?[ 	]*"
					theString)
	(setq theString (replace-match " " nil t theString)))
  theString
  )

(defun ado-strip-double-slash (theString)
  "Strips // comments and everything after them to the end of the line.
Works on a string, returns the result"
  (while 
;	  (string-match "\\([ 	]*[^/]//[^/]+\\|^//[^/]+\\)"
	  (string-match "\\([ 	]*\\)\\([^/]\\)//[^/]+?\\([
]+\\)"
					(concat theString "
"))
	(setq theString (replace-match "\\1\\2\\3" nil nil theString)))
;(message (concat "strip double slash wants to return "  (substring theString 0 -1)))
;  (substring theString 0 -1)
theString
  )

(defun ado-strip-c-comments (theString)
  "Strips /* ... */ comments from a string and returns the
result"
  ;; uses a loop, because I couldn't figure out how to get
  ;;  regexps to handle nested /* ... */ style comments.
  (let ((startPoint 0) (nesting 1) (notDone t) (strippedString "") pareThru)
	(while notDone
	  (setq pareThru (string-match "\\(/[*]\\|[*]/\\)" theString startPoint))
(message (concat "pareThru has been set to " pareThru))
	  (setq strippedString (concat strippedString (substring theString startPoint pareThru)))
	  (if pareThru
		  (progn
			(if (string= (match-string 0 theString) "*/")
				(error "Too many */ in a /* */-style comment"))
			(setq startPoint (match-end 0))
			(while (> nesting 0)
			  (if (not (string-match "\\(/[*]\\|[*]/\\)" theString startPoint))
				  (error "Too many /* in a /* */-style comment"))
			  (setq startPoint (match-end 0))
			  (if (string= (match-string 0 theString) "/*")
				  (setq nesting (1+ nesting))
				(setq nesting (1- nesting))))
			(setq nesting 1))
		(setq notDone nil)))
(message (concat "strippedString is now " strippedString))
	strippedString))

(provide 'ado-clip)
