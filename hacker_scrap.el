;;;  The following is needed if you every want to edit ado-mode.el
;;;    Without them, you will not be able to byte-compile your edited file.
;;;    (They are for the magic of the syntax highlighting.)
;; Autoload the make-regexp and make-regexps functions for easier changes to the auto-highlighting 
(autoload 'make-regexp "make-regexp" 
"Return a regexp to match a string item in STRINGS.") 
(autoload 'make-regexps "make-regexp" 
"Return a regexp to REGEXPS.") 

;; This can be of use for people who like highlighting. You probably 
;; have this set somewhere already.
;;
;; The following line turns on maximum context-sensitive highlighting whenever 
;; you open *any* file, Stata-related or not 
(cond ((fboundp 'global-font-lock-mode) 
       ;; turn on font-locking everywhere (why not?)
       (global-font-lock-mode t) 
       ;; turn on maximum decoration
       (setq font-lock-maximum-decoration t))) 

