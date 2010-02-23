;;; obsolete highlighting snippets---not used as a file, just as a 
;;;   repository
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("mfx") ado-builtin-harmless-face t)
		  "[ \t]+"
		  '(("c" "co" "com" "comp" "compu" "comput" "compute" 
		  "r" "re" "rep" "repl" "repla" "replay" 
		  ) ado-subcommand-face t) 
		  "\\b"
		  ))
