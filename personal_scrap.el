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



