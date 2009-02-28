;;; Mandatory additions --- you must add these to get things working properly 
;; Tell emacs where the ado-mode is
;; You MUST add these two lines to your Emacs customization file, wherever it may be
(setq load-path (cons "/Universal/Custom/emacs/ado-mode/lisp" load-path))
(require 'ado-mode)

;; The following two uncommented lines may be changed from within Emacs' customizations
;; Include in your Emacs customization file only if you like doing such things.

;;; For all of these additions, you can change '/Universal/Custom/emacs' to the folder holding
;;;  the ado-mode folder. Leave the rest of the path as-is, because the nesting is fixed
;; Tell the ado-mode where the templates for new ado-files and other pieces can be found. 
;; These should be globally set for a group, since ado-mode needs some of the files to work properly.
;; You will need to change the directory name to the place where you put the templates folder 
(setq ado-site-template-dir "/Universal/Custom/emacs/ado-mode/templates") 

;; for Mac OS X users, say where the applescript is which can pass information to Stata
;; for non-Mac OS X users, comment out the following line
;; set this for all users; each user can override
(setq ado-script-dir "/Universal/Custom/emacs/ado-mode/scripts")


