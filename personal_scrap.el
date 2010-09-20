;; This is useful if you would like to include syntax hightlighting
;;  of things you've downloaded or have written yourself
;; WARNING! Before you put this in your .emacs file, be sure to run
;; M-x customize-group RET ado-path to set up your (most common)
;;   ado-path
;; If you add these hooks, simply reloading ado-mode will update 
;;  the highlighting for any changes (additions/subtractions)!
(add-hook 'ado-mode
		  (function (lambda ()
					  (ado-add-personal)
					  (ado-add-plus)
					  (ado-add-site)
					  (ado-add-oldplace)
					  ;; for adding an arbitrary folder
					  (ado-modify-font-lock-keywords 'some-arbitrary-name "/Whatever/your/path/might/be" 'whatever-your-face-may-be)
					  )
					))
;
;; All of the following should really be set via M-x customize-group ado RET
;;  but is here for you old-timers.

;;   this gets used when making help files. 
(setq ado-claim-name "Your Name Here") 

;; Copy the ado-signature-template to a location of your choosing, 
;;   change the information, and store it in the file like .ado-signature 
;; Once again, this is definitely for individuals. 
(setq ado-signature-file "~/Custom/emacs/.ado-signature") 

;;; Optional but Highly Recommended Additions 

;; Tell the ado-mode the directory which will hold new, untested Stata programs. 
;; It should be a directory on your adopath within Stata, so that Stata finds
;;  your new ados. This could be what Stata calls your PERSONAL folder.
(setq ado-new-dir "~/Custom/Stata/ado/new/") 

;; A directory to store good value labels. This could be shared across a group
;;; This should also be in your adopath within Stata
(setq ado-label-dir "~/Custom/Stata/labels/") 

;; A holder for your initials (or a short version of your name) if you want
;; your initials or name to show up when you save files
(setq ado-initials "abc")



