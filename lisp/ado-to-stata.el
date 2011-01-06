;;; a collection of things for interacting with Stata 
;;; Currently this works in Mac OS X and MS Windows. It will not cause errors in 
;;;   other OSes. All that is needed for each other OS are some methods
;;;   for talking to Stata from emacs. 
;;;   In Mac OS X, this is done via the applescript send2stata.scpt
;;;   In MS Windows, this is done via the autoit executable send2stata.exe 

(defun ado-send-command-to-stata ()
  (interactive)
  (ado-command-to-clip ado-submit-default)
  (ado-send-clip-to-stata ado-submit-default ado-comeback-flag))

(defun ado-send-command-to-command ()
  (interactive)
  (ado-command-to-clip "command")
  (ado-send-clip-to-stata "command" ado-comeback-flag))

(defun ado-send-command-to-menu ()
  (interactive)
  (ado-command-to-clip "menu")
  (ado-send-clip-to-stata "menu" ado-comeback-flag))

(defun ado-send-command-to-dofile ()
  (interactive)
  (ado-command-to-clip "dofile")
  (ado-send-clip-to-stata "dofile" ado-comeback-flag))

(defun ado-send-clip-to-stata (&optional dothis comeback tmpfile)
  "Sends the clipboard to Stata to be evaluated. Currently this works
on Mac OS X and MS Windows only. This command is meant to be called by 
one of the wrappers determining the behavior of the flags...

There are three optional arguments:
  dothis: \"command\" for using the commmand window
          \"menu\"   for using a menu item
          \"dofile\" for using a tmp do-file

  comeback: if nil, stay in Stata after submitting command; t to come
            back to emacs.

  tmpfile: name of the tmpfile to use if running via temporary do-file
           (not used, just reserved for future use)

By default, you do not need to do any setup. If you play around
with the scripts and want to call something other than what came with 
ado-mode, set \\[ado-script-dir] to point to where your version of 
send2stata.scpt is stored. "
  (interactive)
  (unless dothis (setq dothis ado-submit-default))
  (unless comeback (setq comeback ado-comeback-flag))
  (cond
   ((or (string= dothis "menu") (string= dothis "dofile") (string= dothis "command"))
	(cond 
	 ((string= system-type "darwin")
	  ;; the comeback for Mac OS X is handled via a shell command below
	  (shell-command (concat "osascript '" 
							 (ado-check-a-directory ado-script-dir) 
							 "send2stata.scpt' \"" dothis "\"")))
	 ((string= system-type "windows-nt")
	  ;; autoit can send to non-active windows, so comeback is handled there
	  ;; need to be sure that comeback is a string for concatenation
	  (if comeback (setq comeback "t"))
	  ;;  working via the menu does NOT work with comeback, yet
	  (if (and comeback (string= dothis "menu"))
		  (error "cannot comeback to Stata after using a menu in MS Windows"))
	  ;; for whatever reason, running synchronously causes the autoit
	  ;;  application to do nothing
      ;; the bad news is the damn asynch buffer is shown w/o any choice
	  (call-process-shell-command 
	   (concat 
		(ado-check-a-directory ado-script-dir) 
		   "send2stata.exe \"" dothis "\" \"" comeback "\""
		   " \"" ado-temp-dofile "\""
		   " \"" (unless (= 0 ado-stata-instance) (number-to-string ado-stata-instance)) "\""
		   " \"" ado-stata-version "\""
		   " \"" ado-stata-flavor "\""
		   " \"" (if ado-send-to-all-flag "t" "") "\""
		   " \"" (if ado-strict-match-flag "t" "") "\""
		   " & ")
	   nil 0))
	 (t (message (concat "working via " dothis "s not supported yet in " 
						 (symbol-name system-type)
						 (if (string= dothis "command")
							 ", but you can paste the command in the command window by hand"))))))
   (t (error "Bad value for 'do-this' in ado-send-region-to-stata"))
   )
  ;; comeback cannot be done in applescript very well
  (cond
   ((string= system-type "darwin")
	(if comeback
		(if (> (shell-command (concat "open \"" (substring invocation-directory 0 (string-match "/Contents" invocation-directory)) "\"")) 0)
			(message "had trouble with shell command")))
	(message (concat "selection sent to Stata"))))
  )

(defun ado-check-a-directory (a-dir-name)
  "First checks to see if the directory contained in a-dir-name is non-nil, 
then checks if the contents is a real existing directory. Returns the
proper directory name if correct, otherwise throws an error."
  (let ((a-dir (eval a-dir-name)))
	(if a-dir
		(progn
		 (setq a-dir (file-name-as-directory a-dir))
		 (if (file-exists-p a-dir)
			 a-dir
		   (error "%s" (concat (symbol-name a-dir-name) "'s value: " a-dir " does not exist."))
;		   (message a-dir-name)
		   ))
	  (error "%s" (concat (symbol-name a-dir-name) " is nil")))))

;; should run this only if a region is not selected. If a region is selected the text
;; of the region should be used.

(defun ado-stata-help (&optional at-point)
  "Tries to ask Stata help for the command in the current line, or if 
the optional at-point argument is non-nil, at point."
  (interactive)
  (if at-point
	  (ado-help-at-point-to-clip)
	(ado-help-command-to-clip))
  (ado-send-clip-to-stata ado-submit-default))

(defun ado-help-at-point ()
  (interactive)
  (ado-stata-help t))

(defun ado-help-command ()
  (interactive)
  (ado-stata-help))

(provide 'ado-to-stata)