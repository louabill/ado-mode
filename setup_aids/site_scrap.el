;;; Mandatory additions --- you must add these to get things working properly 
;; Tell emacs where the ado-mode is
;;   the example below is for my setup---you'll have your own path to 
;;   the ado-mode
;; You MUST add these two lines to your Emacs customization file, wherever it may be
(setq load-path (cons "/Universal/Custom/emacs/ado-mode/lisp" load-path))
(require 'ado-mode)

;; The following two uncommented lines may be changed from within Emacs' customizations
;; Include in your Emacs customization file only if you like doing such things.

;;; For all of these additions, you can change '/Universal/Custom/emacs' to 
;;;   the folder holding the ado-mode folder. 
;;; Leave the rest of the path as-is, because the nesting is fixed
;;  If you change where the templates are located, uncomment and fix 
;;;   the following
;;;   (setq ado-site-template-dir "/Universal/Custom/emacs/ado-mode/templates") 

;; If you move where the scripts are found which allow ado-mode to talk to Stata,
;; uncomment and change the following.
;;(setq ado-script-dir "/Universal/Custom/emacs/ado-mode/scripts")


