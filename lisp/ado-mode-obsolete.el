;;; obsolete highlighting snippets---not used anywhere
	  (eval-when-compile
		 (make-regexps
		  "\\b"
		  '(("mfp") ado-builtin-harmless-face t)
		  "[ \t]"
		  '(( 
			  "clogit" "cnreg" "glm" "logistic" "logit" "mlogit"
			  "nbreg" "ologit" "oprobit" "poisson"
			  "probit" "qreg" "regress" "stcox" "streg" "xtgee"
			  ) ado-subcommand-face t) 
		  "\\b"
		  ))

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
