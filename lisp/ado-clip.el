(defun ado-prep-clipboard-old (&optional use-dofile)
;  (interactive)
  "Copies the current region to the clipboard. If there is no region
selected, it trys to grab the command containing the point.
If `use-dofile' is nil, makes the chunk Command-window safe, 
otherwise it is a simple copy to the clipboard.

Any other fixing, such as stripping out blank lines, is up to 
whatever applies the clipboard."
  (let ((mark-even-if-inactive nil)
		(x-select-enable-clipboard t)
		theString)
  (unless use-dofile
	(setq use-dofile "command"))
	(if (and mark-active t
			 (not (= (region-beginning) (region-end))))
		(setq theString (buffer-substring-no-properties (region-beginning) (region-end))) ;; copy region to theString
	  (setq theString (ado-copy-command t)) ;; copy current command b/c there is no region
	  )
	(if (string= use-dofile "command")
		(setq theString (ado-strip-comments theString)))
	(funcall interprogram-cut-function (ado-one-eol theString))))

(defun ado-prep-clipboard (&optional use-dofile)
	(unless use-dofile
	  (setq use-dofile "command"))
	(let ((x-select-enable-clipboard t)
		  (theString (ado-grab-something -1))
		  )
	  (if (string= use-dofile "command")
		  (setq theString (ado-strip-comments theString)))
	  (funcall interprogram-cut-function (ado-one-eol theString))
	))

(defun ado-grab-something (&optional what-code)
  "If a region is selected, return the region.
If what is nil, return the word at or before point.
If no region is selected, and what is -1, return the entire command.
If what is 0, 1, ... return the 0th, 1st, command, where command
  0 is the main command, 1 is the prefix before the command, 2 is 
  the prefix of the prefix, etc. As of yet, only 0 is implemented."
  (interactive)
  (let ((mark-even-if-inactive nil)
		(x-select-enable-clipboard t)
		)
	(if (and mark-active t
			 (not (= (region-beginning) (region-end))))
		(buffer-substring-no-properties (region-beginning) (region-end)) ;; copy region to theString
	  (if what-code
		  (if (< what-code 0) ;; grab entire command
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
			;; need to check value of what-code to really implement peelingn prefix commands
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
		;; what-code is nil; 
		(word-at-point))
	  )))

(defun ado-what-to-clip (&optional where prefix suffix)
  "For putting things like 'search' and 'help' onto the clipboard.
Made to be called from other progams only."
  (let ((x-select-enable-clipboard t))
	(if prefix (setq prefix (concat prefix " ")))
	(if suffix (setq suffix (concat " " suffix)))
	(message (concat prefix (ado-grab-something where) suffix))
	(funcall interprogram-cut-function (ado-one-eol 
										(concat prefix (ado-grab-something where) suffix)))
	))


(defun ado-help-at-point-to-clip ()
  "Puts -help <word-at-point>- on the clipboard/pasteboard. If a region is
selected this is what is sent, instead."
  (interactive)
  (ado-what-to-clip nil "help"))

(defun ado-help-command-to-clip ()
  "Puts help for the current command (but not the prefixes) on the 
clipboard/pasteboard. If a region is selected, this is sent, instead."
  (interactive)
  (ado-what-to-clip 0 "help"))

(defun ado-strip-comments (theString)
  "Strips out all comments from a selection line by line.
These cannot be modularized, because of ordering problems"
  (let ((nesting 0) 
		(returnString "") 
		pareThru
		matchString)
	;; adding the space in case // are the last chars on a line
	(while (setq pareThru (string-match "\\(^///\\| ///\\|^//\\| //\\|/[*]\\|[*]/\\)" theString))
	  (setq matchString (match-string 1 theString))
	  (if (string= "*/" matchString)
		  (error "Too many */ in a /* */-style comment"))
	  ;; found something to investigate
	  ;; put upto match onto the return string
	  (setq returnString (concat returnString (substring theString 0 pareThru)))
	  (setq theString (substring theString (+ pareThru (length matchString))))
	  (cond 
	   ((or (string= matchString "///") (string= matchString " ///"))
		(if (setq pareThru (string-match "
" theString))
			(setq theString (substring theString (1+ pareThru)))
		  (error "Found /// with no continuation")))
	   ((or (string= matchString "//") (string= matchString " //"))
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
;;  (message (concat "ado-one-eol received ->" theString "<-"))
  (unless (string-match "\\(\\(.\\|[\n]\\)*\\)\\([\n]+\\)" theString)
	(setq theString (concat theString "
")))
;;  (message (concat "ado-one-eol wants to return ->" theString "<-"))
  theString
  )

(provide 'ado-clip)
