(defun ado-copy-command (&optional asString)
  "Copies one of many
If whole is non-nil, copies the entire command line instead 
of just the word."
  (interactive)
  (let ((start-here
		 (save-excursion
		   (ado-beginning-of-command)
		   (point)))
		(end-here
		 (save-excursion
		   (ado-end-of-command)
		   (point)))
		 (x-select-enable-clipboard t))
	(if asString
		(filter-buffer-substring start-here end-here nil t)
	  (kill-ring-save start-here end-here))
	))

(defun ado-prep-clipboard (&optional use-dofile)
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

(defun ado-command-in-line ()
  "Tries to find the command in the current line, and puts the
command on the pasteboard/clipboard. It tries to peel off all
prefix commands; perhaps this behavior will change in the future.
This is deprecated before its release...use \\[ado-grab-whatever]
instead!"
  (interactive)
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

(defun ado-grab-whatever (&optional at-point)
  "Copies one of the following, with the action selected in
the following order:
1. If a region is selected, copies the region.
2. If no region is selected, and at-point is non-nil, copies the word
   at or before point.
3. If no region is selected, and at-point nil, copies the
   copies the entire commmand containing the point.
Note that if a region is selected the value of whole is ignored."
  (interactive)
  (let ((mark-even-if-inactive nil)
		(x-select-enable-clipboard t)
		theString)
	(if (and mark-active t
			 (not (= (region-beginning) (region-end))))
		(buffer-substring-no-properties (region-beginning) (region-end)) ;; return the region
	  (if at-point
		  (setq theString (ado-copy-command t)) ;; copy current command b/c there is no region


	(funcall interprogram-cut-function (ado-one-eol 
										(concat "help "
												(if at-point (word-at-point)
												  (ado-command-in-line)))))))

(defun ado-help-to-clip (&optional at-point)
  "Puts -help <cmd>- on the clipboard/pasteboard. If a region
is selected, the region is used. If there is no region and at-point
is nil, this grabs the first word after the prefix commands. If at-point
is non-nil, it uses the word at point, instead."
  (let ((mark-even-if-inactive nil)
		(x-select-enable-clipboard t))
	(funcall interprogram-cut-function (ado-one-eol 
										(concat "help "
												(if at-point (word-at-point)
												  (ado-command-in-line)))))))

(defun ado-help-to-clip-cmd-at-point ()
  "Asks Stata for help on the word at point."
  (interactive)
  (ado-help-to-clip t))

(defun ado-help-to-clip-cmd-in-line ()
  "Asks Stata for help on the first non-prefix command in the current
command."
  (interactive)
  (ado-help-to-clip))


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
