;;; a collection of things for interacting with Stata 
;;; Right now all this works only in Mac OS X. It will not cause errors in 
;;;   other OSes. All that is needed for each other OS are some methods
;;;   for talking to Stata from emacs. In Mac OS X, this is done via applescript

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
  "Sends the clipboard to Stata to be evaluated. Currently this is a Mac-only 
function. For it to work properly, you need to set the \\[ado-script-dir] 
to point to where the script send2tmpdo.scpt is stored. Should be called 
by one of the wrappers determining the behavior of the flags...

There are three optional arguments:
  dothis: \"command\" for using the commmand window
          \"menu\"   for using a menu item
          \"dofile\" for using a tmp do-file

  comeback: if nil, stay in Stata after submitting command; t to come
            back to emacs.

  tmpfile: name of the tmpfile to use if running via temporary do-file
           (optional, because it isn't really needed...)"
  (interactive)
  (cond
   ((string= dothis "menu")
	(cond 
	 ((string= system-type "darwin")
	  (shell-command (concat "osascript " (ado-check-a-directory ado-script-dir) "send2stata.scpt \"menu\" \"" tmpfile "\"")))
	 (t (message (concat "working via menus not supported yet in " (symbol-name system-type))))))
   ((string= dothis "dofile")
	(cond
	 ((string= system-type "darwin")
	  ;; the following 3 lines should be common to all os's once implemented
	  (unless tmpfile
		(setq tmpfile (concat temporary-file-directory "feedStata.do")))
	  (write-region (x-selection-value 'CLIPBOARD) nil tmpfile)
		;; ado-stata-flavors and ado-stata-home are needed if there are many Statas installed
		(if (string= ado-stata-flavors "")
			  (shell-command (concat "open -a " tmpfile))
			(shell-command (concat "open -a " (ado-check-a-directory ado-stata-home) "Stata" ado-stata-flavors ".app " tmpfile))
			))
	 (t (message (concat "working via temp do-files not supported yet in " (symbol-name system-type))))))
   ((string= dothis "command")
	(cond
	 ((string= system-type "darwin")
	  (shell-command (concat "osascript " (ado-check-a-directory ado-script-dir) "send2stata.scpt \"command\"")))
	 (t (message (concat "working via the command window not yet supported in " (symbol-name system-type) ", but you can paste the command in the command window by hand.")))))
   (t (error "Bad value for 'do-this' in ado-send-region-to-stata"))
   )
  (cond
   ((string= system-type "darwin")
	(if comeback
		(if (> (shell-command (concat "open \"" (substring invocation-directory 0 (string-match "/Contents" invocation-directory)) "\"")) 0)
			(message "had trouble with shell command")))
	(message (concat "selection sent to Stata" ado-stata-flavors))))
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