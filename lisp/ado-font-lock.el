;;; ado-font-lock.el --- all the endless font locking
;; Copyright (c) 2003--2010
;; Bill Rising
;; Author:   Bill Rising
;; Maintainer: Same <brising@alum.mit.edu>
;;             URL: http://homepage.mac.com/brising
;; Keywords: ado-mode
;; Version:  1.11.1.0 of 14jul2010

;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description:
;;
;; This file contains functions for good keyword highlighting, aka
;;   font-locking.
;; The main, huge function is: 
;;   ado-set-font-lock-keywords
;; It is a massively long list of items for the nice highlighting
;;   in ado-mode, nothing more, nothing less
;; Other functions have been added to allow users to include their
;;   own commands in the font locking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun ado-set-font-lock-keywords ()
;  (make-local-variable 'ado-font-lock-keywords)
  (interactive)
  (setq
   ado-font-lock-keywords
   (list
    ;; nested quotes
	(list "\\(`\".*?\"'\\)" '(1 ado-string-face t))
	;; simple *-style comments
	(list "^[ \t]*\\([*].*\\)" '(1 ado-comment-face t))
    ;; special highlighting
	 ;; starting a mata program; not allowing comments, though
	(list "^[ \t]*\\(mata\\)\\(:\\)[ \t]*$" 
		  '(1 ado-builtin-harmful-face)
		  '(2 ado-constant-face))
    ;; ado 'which' comments
	(list "\\(^\\*!.*\\)" '(1 ado-builtin-harmful-face t))
	;; program define or list
	(list
	  (concat
	   "^[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"pr" "pro" "prog" "progr" "progra" "program")
		  'words))
	   "[ \t]+"
	   (eval-when-compile
		 (regexp-opt 
		  '(
			"d" "de" "def" "defi" "defin" "define" "drop"
			"l" "li" "lis" "list"
			) 'words))
	   "[ \t]+\\([_a-zA-Z.]+[_a-zA-Z0-9]*\\)"
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t) '(3 ado-builtin-harmful-face t))
;;
	;; program w/o define
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt '(
			"pr" "pro" "prog" "progr" "progra" "program")
		  'words))
	   "[ \t]+\\([^ \t]+\\)"
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-builtin-harmful-face))
		 
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt '(
			"pr" "pro" "prog" "progr" "progra" "program")
		  'words))
	   "[ \t]+"
	   (eval-when-compile
		 (regexp-opt '("di" "dir")
					 'words))
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t))

	;; it appears Stata accepts any version number
	;; this just allows major[.0/max for particular version]
	;; only 0's: 1, 4, 5, 7
    ;; .1's: 2, 3, 6, 10, 11
	;; .2's: 8, 9
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
		  '("vers" "versi" "versio" "version")
		  'words))
	   "[ \t]+\\(\\(?:\\(?:[1-9]\\|1[01]\\)?[.]0\\)\\|\\(?:\\(?:[23689]\\|1[01]\\)[.]1\\)\\|[89][.]2\\|\\(?:\\(?:[1-9]\\|1[01]\\)?[^.]\\)\\)\\b"
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))
	;; pause on/off
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '("pause") 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '("off" "on") 'words))
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))
	  ;; end and pause must start lines
	(list
	 (concat
	  "^[ \t]*"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "end" "pause"
		   ) 'words))
	  )
	 '(1 ado-builtin-harmful-face))

	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "#d" "#de" "#del" "#deli" "#delim" "#delimi" "#delimit" 
		 ) 'words))
	   "[ \t]+\\(cr\\|;\\)[ \t]*$"
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

    ;; obsolete stuff which appears as OK as subcommands
	(list
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "lfit"
		 "sco" "scor" "score"
		 ) 'words))
	  '(1 ado-obsolete-face))

    ;; the cluster commands
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt '("cluster") 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt '(
			 "dend" "dendr" "dendro" "dendrog" "dendrogr" "dendrogra" "dendrogram"
			 "dir"
			 "k" "km" "kme" "kmea" "kmean" "kmeans" 
			 "kmed" "kmedi" "kmedia" "kmedian" "kmedians" 
			 "list"
			 "note" "notes"
			 "parsedist" "parsedista" "parsedistan" "parsedistanc" "parsedistance" 
			 "query"
			 "stop"
			 "tr" "tre" "tree"
			 ) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

    ;; 

	(list
	 (concat
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "cluster"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "del" "dele" "delet" "delete" 
		   "drop"
		   "gen" "gene" "gener" "genera" "generat" "generate" 
		   "measures"
		   "rename" "renamevar"
		   "set"
		   "use"
		   ) 'words))
	  )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face))


    ;; putting together common cluster and clustermat commands

	(list
	 (concat
	  "[ /t]*\\<\\(cluster\\(?:mat\\)?\\)\\>[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "a" "anova"
		   "av" "ave" "aver" "avera" "averag" "average" "averagel" "averageli" 
		   "averagelin" "averagelink" "averagelinka" "averagelinkag" "averagelinkage" 
		   "c" "cent" "centr" "centro" "centroi" "centroid" "centroidl" "centroidli" 
		   "centroidlin" "centroidlink" "centroidlinka" "centroidlinkag" "centroidlinkage" 
		   "co" "com" "comp" "compl" "comple" "complet" "complete" "completel" "completeli" 
		   "completelin" "completelink" "completelinka" "completelinkag" "completelinkage" 
		   "manova" "med" "medi" "media" "median" "medianl" "medianli" "medianlin" 
		   "medianlink" "medianlinka" "medianlinkag" "medianlinkage" 
		   "s" "si" "sin" "sing" "singl" "single" "singlel" "singleli" "singlelin" 
		   "singlelink" "singlelinka" "singlelinkag" "singlelinkage"
		   "ward" "wards" "wardsl" "wardsli" "wardslin" "wardslink" "wardslinka" "wardslinkag" "wardslinkage"
		   "wav" "wave" "waver" "wavera" "waverag" "waverage" "waveragel" 
		   "waverageli" "waveragelin" "waveragelink" "waveragelinka" "waveragelinkag" "waveragelinkage" 
		   ) 'words))
	  )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

	 ;; discrim commands
	(list
	 (concat
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "discrim"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "knn"
		   "lda"
		   "logistic"
		   "qda"
		   ) 'words))
	  )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

	 ;; stpower commands
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "stpow" "stpowe" "stpower" 
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			 "cox"
			 "exp" "expo" "expon" "expone" "exponen" "exponent" "exponenti" "exponentia" "exponential" 
			 "log" "logr" "logra" "logran" "logrank" 
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))


	;; set command and its variations
	;; splitting up what was done before

	(list
	  (concat
	   "^[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "se" "set"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"a" "ad" "ado" "ados" "adosi" "adosiz" "adosize" 
			"autotabgraphs"
			"cformat"
			"httpproxyhost" "httpproxyport" "httpproxypw" "httpproxyuser"
			"l" "le" "lev" "leve" "level"
			"li" "lin" "line" 
			"lineg" "linega" "linegap" 
			"lines" "linesi" "linesiz" "linesize" 
			 "mat" "mats" "matsi" "matsiz" "matsize"
			 "maxdb" "maxiter" "maxvar"
			 "mem" "memo" "memor" "memory"
			 "notifyuser"
			 "ob" "obs"
			 "pa" "pag" "page" "pages" "pagesi" "pagesiz" "pagesize"
			 "pformat"
			 "processors"
			 "reventr" "reventri" "reventrie" "reventries" 
			 "scheme" "scrollbufsize"
			 "se" "see" "seed"
			 "sformat"
			 "smoothsize"
			 "timeout1"
			 "timeout2"
			 "traced" "tracede" "tracedep" "tracedept" "tracedepth" 
			 "traceh" "tracehi" "tracehil" "tracehili" "tracehilit" "tracehilite" 
			 "update_interval"
			 "varlab" "varlabe" "varlabel" 
			 "varlabelpos"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	;; set with odd options
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "se" "set"
		 ) 'words))
	   "[ \t]+"
	   "\\<\\("
	   "\\(?:conren\\(?:[ \t]+\\(?:clear\\|sf\\|bf\\|it\\|res\\|resu\\|resul\\|result\\|reset\\|txt\\|text\\|inp\\|inpu\\|input\\|err\\|erro\\|error\\|li\\|lin\\|link\\|hi\\|hil\\|hili\\|hilit\\|hilite\\|uloff\\|ulon\\)\\)?\\)"
	   "\\|"
	   "\\(?:copycolor[ \t]+\\(?:auto\\|autom\\|automa\\|automat\\|automati\\|automatic\\|asis\\|gs[123]\\)\\)"
	   "\\|"
	   "\\(?:dp[ \t]+\\(?:com\\|comm\\|comma\\|per\\|peri\\|perio\\|period\\)\\)"
	   "\\|"
	   "\\(?:emptycells[ \t]+\\(?:keep\\|drop\\)\\)"
	   "\\|"
	   "\\(?:eolc\\(?:h\\|ha\\|har\\)[ \t]+\\(?:mac\\|unix\\)\\)"
	   "\\|"
	   "\\(?:log\\(?:t\\|ty\\|typ\\|type\\)[ \t]+\\(?:t\\|te\\|tex\\|text\\|s\\|sm\\|smc\\|smcl\\)\\)"
	   "\\|"
	   "\\(?:odbcm\\(?:\\g\\|gr\\)[ \t]+\\(?:iodbc\\|unixodbc\\)\\)"
	   "\\|"
	   "\\(?:printcolor[ \t]+\\(?:auto\\|autom\\|automa\\|automat\\|automati\\|automatic\\|asis\\|gs[123]\\)\\)"
	   "\\|"
	   "\\(?:search\\(?:d\\|de\\|def\\|defa\\|defau\\|defaul\\|default\\)[ \t]+\\(?:all\\|local\\|net\\)\\)"
	   "\\|"
	   "\\(?:showbaselevels[ \t]+\\(?:o\\(?:ff\\|n\\)\\|all\\)\\)"
	   "\\|"
	   "\\(?:t\\(?:y\\|yp\\|ype\\)[ \t]+\\(?:double\\|float\\)\\)"
	   "\\)\\>")
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))
   
	;; set <foo> on/off commands

	(list
	  (concat
	   "^[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "se" "set"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "checksum" "fastscroll"
		  "dockable"
		  "dockingg" "dockinggu" "dockinggui" "dockingguid" "dockingguide" "dockingguides"
		  "doublebuffer"
		  "floatresults" "floatwindows"
		  "g" "gr" "gra" "grap" "graph" "graphi" "graphic" "graphics"
		  "httpproxy" 
		  "httpproxya" "httpproxyau" "httpproxyaut" "httpproxyauth" 
		  "locksplit" "locksplitt" "locksplitte" "locksplitter" "locksplitters" 
		  "mo" "mor" "more" 
		  "persistfv" "persistvtopic"
		  "pinnable"
		  "playsnd"
		  "r" "revkeyboard" "rm" "rms" "rmsg"
		  "smoothf" "smoothfo" "smoothfon" "smoothfont" "smoothfonts" 
		  "tr" "tra" "trac" "trace"
		  "tracee" "traceex" "traceexp" "traceexpa" "traceexpan" "traceexpand" 
		  "tracei" "tracein" "traceind" "traceinde" "traceinden" "traceindent" 
		  "tracen" "tracenu" "tracenum" "tracenumb" "tracenumbe" "tracenumber" 
		  "traces" "tracese" "tracesep" 
		  "update_prompt" "update_query"
		  "varabbrev" "varkeyboard"
		  "vir" "virt" "virtu" "virtua" "virtual"
		  "xptheme"
			) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"off" "on"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t) '(3 ado-subcommand-face t))

	;

	;; set output command, with its odd subsubcommands
	(list
	 (concat
	  "^[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "se" "set"
		 ) 'words))
	   "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "ou" "out" "outp" "outpu" "output"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"e" "er" "err" "erro" "error"
			"i" "in" "inf" "info" "infor" "inform" 
			"p" "pr" "pro" "proc" 
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face) '(3 ado-subcommand-face))

	;; ;; set showbaselevels command with it's odd all subcommand
	;; (list
	;;  (concat
	;;   "^[ \t]*"
	;;    (eval-when-compile 
	;; 	 (regexp-opt 
    ;;    '(
	;; 	 "se" "set"
	;; 	 ) 'words))
	;;    "[ \t]+"
	;;    "\\<\\(showbaselevels\\)\\>"
	;;    "[ \t]+"
	;;    (eval-when-compile 
	;; 	 (regexp-opt 
	;; 	  '(
	;; 		"all" "off" "on"
	;; 		) 'words))
	;;    )
	;;   '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face) '(3 ado-subcommand-face t))

	;; these appear to be obsolete and do not show in the manuals
	;;   some work, however.
	(list
	 (concat
	  "^[ \t]*"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "se" "set"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "ANSI" 
		   "b" "be" "bee" "beep" "contents" 
		   "d" "di" "dis" "disp" "displ" "displa" "display"
		   "help"
		   "IBM" 
		   "icmap"
		   "log"
		   "macgp" "macgph" "macgphe" "macgphen" "macgpheng" 
		   "macgphengi" "macgphengin" "macgphengine" 	  
		   "maxobs"
		   "piccom" "piccomm" "piccomme" "piccommen" "piccomment" "piccomments" 
		   "printcolor[ \t]+grayscale"
		   "seed0" "shell" "smalldlg"
		   "te" "tex" "text" "texts" "textsi" "textsiz" "textsize"
		   "use_atsui_graph" "use_qd_text"
		   "varwin" "varwind" "varwindo" "varwindow" "video"
		   ) 'words))
	  )
	 '(1 ado-builtin-harmless-face) '(2 ado-obsolete-face t))

	  ;; the timer command
	(list
	 (concat
	  (eval-when-compile 
		(regexp-opt 
		 '(
		 "timer"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"clear" "list"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "timer"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"clear" "list" "off" "on"
			) 'words))
	   "[ \t]+"
	   "\\([0-9]+\\|`[a-zA-Z0-9_`']'\\)"
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face) '(3 ado-subcommand-face))
		  
     ;; the args command 

	(list
	 (concat
	  "\\(\\<args\\>\\)"
	  "[ \t]+"
 	  "\\(\\(?:[a-zA-Z_][a-zA-Z_0-9]*[ \t]*\\)+\\)"
	  )
	 '(1 ado-builtin-harmful-face) '(2 ado-variable-name-face t))

     ;; char with sub commands
	(list
	 (concat
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "char"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "l" "li" "lis" "list" 
		   ) 'words))
	  )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

	(list
	 (concat
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "char"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "define"
		   "ren" "rena" "renam" "rename"
		   ) 'words))
	  )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face))

    ;; the constraint commands
	(list
	 (concat
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "cons" "const" "constr" "constra" "constrai" "constrain" "constraint" 
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "d"
		   "de" "def" "defi" "defin" "define" 
		   "di" "dir"
		   "drop"
		   "free" "get"
		   "l" "li" "lis" "list"
		   ) 'words))
	  )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

    ;; the confirm commands - could be a mess!
	(list
	 (concat
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "conf" "confi" "confir" "confirm"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "e" "ex" "exi" "exis" "exist" "existe" "existen" "existenc" "existence"
		   "f" "fi" "fil" "file"
		   "fo" "for" "form" "forma" "format" 
		   "mat" "matr" "matri" "matrix" 
		   "n" "name" "names" "nu" "num" "numb" "numbe" "number" 
		   "sca" "scal" "scala" "scalar" 
		   "v" "va" "var" "vari" "varia" "variab" "variabl" "variable"
		   ) 'words))
	  )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "conf" "confi" "confir" "confirm"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
	  "integer"
			) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
	  "n" "nu" "num" "numb" "numbe" "number"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face) '(3 ado-subcommand-face))

	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "conf" "confi" "confir" "confirm"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
	  "n" "ne" "new"
			) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
	  "f" "fi" "fil" "file"
	  "v" "va" "var" "vari" "varia" "variab" "variabl" "variable"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face) '(3 ado-subcommand-face))

	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "conf" "confi" "confir" "confirm"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
	  "byte" "double" "float" "int" "long"
	  "numeric"
	  "str" "stri" "strin" "string"
			) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
	  "v" "va" "var" "vari" "varia" "variab" "variabl" "variable"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face) '(3 ado-subcommand-face))

	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "conf" "confi" "confir" "confirm"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			 "date"
			 "numeric"
			 "str" "stri" "strin" "string"
			 "ts"
			) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			 "fo" "for" "form" "forma" "format" 
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face) '(3 ado-subcommand-face))

	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "conf" "confi" "confir" "confirm"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			 "ts"
			) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			 "fo" "for" "form" "forma" "format" 
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-obsolete-face) '(3 ado-subcommand-face))

    ;;; confirm str# 
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "conf" "confi" "confir" "confirm"
		 ) 'words))
	   "[ \t]+"
	   "\\<\\(str"
	   "\\|"
	   "\\(?:str[1-9][0-9]?\\)" 
	   "\\|"
	   "\\(?:str1[0-9][0-9]\\)" 
	   "\\|"
	   "\\(?:str2[0-3][0-9]\\)" 
	   "\\|"
	   "\\(?:str24[0-4]\\)"
	   "\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "v" "va" "var" "vari" "varia" "variab" "variabl" "variable" 
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face) '(3 ado-subcommand-face))

	;; merge
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "mer" "merg" "merge"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"1:1" "1:m" "m:1" "m:m"
			) 'words))
	   )
	  '(1 ado-builtin-harmful-face t) '(2 ado-subcommand-face))

	;; mvtest
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "mvtest"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "corr" "corre" "correl" "correla" "correlat" "correlati" "correlatio" "correlation" "correlations" 
		  "cov" "cova" "covar" "covari" "covaria" "covarian" "covarianc" "covariance" "covariances" 
		  "m" "me" "mea" "mean" "means" 
		  "norm" "norma" "normal" "normali" "normalit" "normality"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face))

	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "note" "notes" 
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"drop"
			"renumber" "replace"
			"search"
			) 'words))
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "note" "notes" 
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "l" "li" "lis" "list" 
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

    ;; the duplicates commands
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "duplicates"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			 "e" "ex" "exa" "exam" "examp" "exampl" "example" "examples" 
			 "l" "li" "lis" "list" 
			 "r" "re" "rep" "repo" "repor" "report" 
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "duplicates"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			 "drop"
			 "t" "ta" "tag" 
			) 'words))
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face))

	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "duplicates"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			 "b" "br" "bro" "brow" "brows" "browse" 
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-obsolete-face))

	;; estimates commands moved to just after estat commands
    ;; the _estimates commands
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
	  "_est" "_esti" "_estim" "_estima" "_estimat" "_estimate" "_estimates"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
	  "clear"
	  "dir" 
	  "drop"
	  "h" "ho" "hol" "hold"
	  "u" "un" "unh" "unho" "unhol" "unhold"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t))

    ;; the estat commands --- moved to after the obsolete commands

    ;; the file commands
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "file"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			  "close" "open" 
			  "q" "qu" "que" "quer" "query" 
			  "r" "re" "rea" "read" 
			  "seek" 
			  "set"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "file"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
	   "sersetread" "sersetwrite"
	   "w" "wr" "wri" "writ" "write" 
			) 'words))
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face))

    ;; the gph commands
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "gph"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
	  "arc"
	  "box"
	  "clear" "close"
	  "font"
	  "line"
	  "open"
	  "pen" "point"
	  "text"
	  "vline" "vpoint" "vpoly" "vtext"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))
    ;;
    ;; the gprefs commands
    ;;   (in multiple pieces)
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "gprefs"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"q" "qu" "que" "quer" "query" 
			) 'words))
	   )
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"window"
			) 'words))
	   	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face) '(3 ado-subcommand-face))
	(list
	  (concat
	   "\\<\\(gprefs\\)\\>"
	   "[ \t]+"
	   "\\<\\(set\\)\\>"
	   "[ \t]+"
	   "\\<\\(window\\)\\>"
	   "[ \t]+"
	   "\\<\\(scheme\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			  "black" "blackb" "blackbg" 
			  "custom1" "custom2" "custom3"
			  "mono" "monoc" "monoch" "monochr" "monochro" "monochrom" "monochrome" 
			  "white" "whiteb" "whitebg" 
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face)
	  '(3 ado-subcommand-face) '(4 ado-subcommand-face) '(5 ado-subcommand-face))

     ;;
     ;; the other gprefs set window 
	(list
	  (concat
	   "\\<\\(gprefs\\)\\>"
	   "[ \t]+"
	   "\\<\\(set\\)\\>"
	   "[ \t]+"
	   "\\<\\(window\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			  "update"
			  "xsize"
			  "ysize"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face)
	  '(3 ado-subcommand-face) '(4 ado-subcommand-face))

	(list
	  (concat
	   "\\<\\(gprefs\\)\\>"
	   "[ \t]+"
	   "\\<\\(set\\)\\>"
	   "[ \t]+"
	   "\\<\\(window\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			  "displaythick"
			  "usegphsize"
			) 'words))
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			  "off" "on"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face)
	  '(3 ado-subcommand-face) '(4 ado-subcommand-face) '(5 ado-subcommand-face))
     ;;
     ;; the gprefs set scheme commands 
	(list
	  (concat
	   "\\<\\(gprefs\\)\\>"
	   "[ \t]+"
	   "\\<\\(set\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			  "custom1" "custom2" "custom3"
			  ) 'words))
        "[ \t]+"
		(eval-when-compile
		  (regexp-opt
		   '(
			  "background_color"
			  "pen1_color" "pen2_color" "pen3_color" "pen4_color" "pen5_color" "pen6_color" "pen7_color" "pen8_color" "pen9_color"
			  "pen1_thick" "pen2_thick" "pen3_thick" "pen4_thick" "pen5_thick" "pen6_thick" "pen7_thick" "pen8_thick" "pen9_thick"
			  "symmag_all"
			  ) 'words))
		)
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face)
	  '(3 ado-subcommand-face) '(4 ado-subcommand-face))
     ;;
     ;; shoulda never started this - the gprefs query scheme layout all by its lonesome
	(list
	  (concat
	   "\\<\\(gprefs\\)\\>"
	   "[ \t]+"
	   "\\<\\(query\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			  "custom1" "custom2" "custom3"
			  ) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face)
	  '(3 ado-subcommand-face))

     ;;; worst than smcl ---- it's graph!
     ;;;  -> will need multiple copies of the subcommands for the () and || and plain versions
     ;;;     argh, what a pain in the rear.
	  ;; regular graph ... commands
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "gr" "gra" "grap" "graph"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			  "bar" "box"
			  "combine" "copy"
			  "des" "desc" "descr" "descri" "describ" "describe" 
			  "di" "dir" "dis" "disp" "displ" "displa" "display"
			  "dot"
			  "export"
			  "hbar" "hbox"
			  "matrix"
			  "pie" "play" "print"
			  "q" "qu" "que" "quer" "query" 
			  "save" "set"
			  "tw" "two" "twow" "twowa" "twoway"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t))

	  ;; the initial graph commands which are destructive
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "gr" "gra" "grap" "graph"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "drop"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "_all"
		 ) 'words))
	   )
	  '(1 ado-builtin-harmful-face t) '(2 ado-subcommand-face t) '(3 ado-subcommand-face t))

	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "gr" "gra" "grap" "graph"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "drop" "rename" "use"
		 ) 'words))
	   )
	  '(1 ado-builtin-harmful-face t) '(2 ado-subcommand-face t))
	  ;; graph set commands ! seem to introduce all sorts of trouble
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "gr" "gra" "grap" "graph"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"set"
			) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"eps" "print" "ps"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t) '(3 ado-subcommand-face t))
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "gr" "gra" "grap" "graph"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"set"
			) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"window"
			) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"fontface" "fontfacemono" "fontfacesans"
			"fontfaceserif" "fontfacesymbol"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t) 
	  '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))

	  ;; the graph twoway stuff 
	(list
	 (concat
	  "\\<\\(\\(?:\\(?:gr\\|gra\\|grap\\|graph\\)[ \t]+\\)?\\)"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "tw" "two" "twow" "twowa" "twoway"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			  "area"
			  "bar"
			  "con" "conn" "conne" "connec" "connect" "connecte" "connected" 
			  "dot" "dropline"
			  "fpfit" "fpfitci" "function"
			  "hist" "histo" "histog" "histogr" "histogra" "histogram" 
			  "kdensity"
			  "line"
			  "lfit" "lfitci"
			  "lowess" "lpoly" "lpolyci"
 			  "mband" "mspline"
			  "pcarrow" "pcarrowi" "pcbarrow" "pcbarrowi" "pccapsym" "pci" "pcscatter" "pcspike"
			  "qfit" "qfitci"
			  "rarea" "rbar" "rcap" "rcapsym" 
			  "rcon" "rconn" "rconne" "rconnec" "rconnect" "rconnecte" "rconnected" 
			  "rl" "rli" "rlin" "rline" 
			  "rsc" "rsca" "rscat" "rscatt" "rscatte" "rscatter" 
			  "rspike"
			  "sc" "sca" "scat" "scatt" "scatte" "scatter" 
			  "scatteri" "spike"
			  "tsline" "tsrline"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face t) 
	  '(3 ado-subcommand-face t))

	  ;; even more aggravating: things for which both graph and twoway are optional
	  
     ;; icd9, icd9p commands
	(list
	 (concat
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "icd9" "icd9p"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "check"
		   "l" "lo" "loo" "look" "looku" "lookup"
		   "q" "qu" "que" "quer" "query" 
		   "sea" "sear" "searc" "search" 
		   ) 'words))
	  )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

     ;; icd9s with generate
	(list
	 (concat
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "icd9" "icd9p"
		   ) 'words))
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "clean"
		   "gen" "gene" "gener" "genera" "generat" "generate" 
		   ) 'words))
	  )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face))
	
	 ;; fvset commands
	(list
	  (concat
	   "\\<\\(fvset\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			 "b" "ba" "bas" "base" 
			 "clear"
			 "d" "de" "des" "desi" "desig" "design" 
			 "report"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

	 ;; log commands, cmdlog also (though cmdlog query is undocumented)
	(list
	  (concat
	   (eval-when-compile
		 (regexp-opt
		  '(
			"cmdlog" "log"
			) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			 "c" "cl" "clo" "clos" "close" 
			 "of" "off" "on"
			 "query"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

	 ;; misstable commands
	(list
	  (concat
	   "\\<\\(misstable\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			 "nest" "neste" "nested"
			 "sum" "summ" "summa" "summar" "summari" "summariz" "summarize" 
			 "pat" "patt" "patte" "patter" "pattern" "patterns" 
			 "tree"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

     ;; some of the matrix commands
     ;; with no matrix arguments - harmless
     ;; with one following argument but no subcommand
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "mat" "matname" "mat_put_rr" "matr" "matri" "matrix"
		 ) 'words))
	   "[ \t]+"
	   "\\([a-zA-Z]+[a-zA-Z0-9_]*\\)\\b"
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-matrix-name-face))
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "mat" "matr" "matri" "matrix"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"d" "di" "dir" "dispCns"
			"post"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))
     ;; with no matrix arguments - harmful
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "mat" "matr" "matri" "matrix"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"sco" "scor" "score"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "mat" "matr" "matri" "matrix"
		 ) 'words))
	   "[ \t]+"
	   "\\<\\(makeCns\\)\\>"
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face))
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "mat" "matr" "matri" "matrix"
		 ) 'words))
	   "[ \t]+"
	   "\\<\\(drop\\)\\>"
	   "[ \t]+"
	   "\\<\\(_all\\)\\>"
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face)
	  '(3 ado-subcommand-face))
	;; doesn't quite work, because it underlines the spaces
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "mat" "matr" "matri" "matrix"
		 ) 'words))
	   "[ \t]+"
	   "\\<\\(drop\\)\\>"
	   "\\(\\(?:[ \t]+[a-zA-Z][a-zA-Z0-9_]*\\)+\\)"
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face) '(3 ado-matrix-name-face))

     ;; with one following argument
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "mat" "matr" "matri" "matrix"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			  "ac" "acc" "accu" "accum" 
			  "cole" "coleq" 
			  "coln" "colna" "colnam" "cloname" "colnames"
			  "def" "defi" "defin" "define"
			  "dis" "diss" "dissi" "dissim" "dissimi" "dissimil" "dissimila" "dissimilar" 
			  "dissimilari" "dissimilarit" "dissimilarity" 
			  "glsa" "glsac" "glsacc" "glsaccu" "glsaccum"
			  "in" "inp" "inpu" "input" 
			  "opaccum"
			  "rowe" "roweq" 
			  "rown" "rowna" "rownam" "rowname" "rownames"
			  "veca" "vecac" "vecacc" "vecaccu" "vecaccum"
			) 'words))
	   "[ \t]+"
	   "\\([a-zA-Z]+[a-zA-Z0-9_]*\\)\\b"
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	  '(3 ado-matrix-name-face t))
     ;; with one following arguments -- but harmless (good grief!)
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "mat" "matr" "matri" "matrix"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			  "l" "li" "lis" "list" 
			) 'words))
	   "[ \t]+"
	   "\\([a-zA-Z]+[a-zA-Z0-9_]*\\)\\b"
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t)
	  '(3 ado-matrix-name-face t))
     ;; with two following arguments
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "mat" "matr" "matri" "matrix"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			  "eigenval" "eigenvalu" "eigenvalue" "eigenvalues" 
			  "ren" "rena" "renam" "rename" 
			  "syme" "symei" "symeig" "symeige" "symeigen"
			) 'words))
	   "[ \t]+"
	   "\\<\\([a-zA-Z]+[a-zA-Z0-9_]*\\)\\>"
	   "[ \t]+"
	   "\\<\\([a-zA-Z]+[a-zA-Z0-9_]*\\)\\>"
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	  '(3 ado-matrix-name-face t) '(4 ado-matrix-name-face t))
     ;; with three(!) following arguments
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "mat" "matr" "matri" "matrix"
		 ) 'words))
	   "[ \t]+"
	   "\\<\\(svd\\)\\>"
	   "[ \t]+"
	   "\\<\\([a-zA-Z]+[a-zA-Z0-9_]*\\)\\>"
	   "[ \t]+"
	   "\\<\\([a-zA-Z]+[a-zA-Z0-9_]*\\)\\>"
	   "[ \t]+"
	   "\\<\\([a-zA-Z]+[a-zA-Z0-9_]*\\)\\>"
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	  '(3 ado-matrix-name-face t) '(4 ado-matrix-name-face t)
	  '(5 ado-matrix-name-face t))
     ;; with three(!) following arguments but no friggin matrix command! 
	(list
	  (concat
	   "\\<\\(matcproc\\)\\>"
	   "[ \t]+"
	   "\\<\\([a-zA-Z]+[a-zA-Z0-9_]*\\)\\>"
	   "[ \t]+"
	   "\\<\\([a-zA-Z]+[a-zA-Z0-9_]*\\)\\>"
	   "[ \t]+"
	   "\\<\\([a-zA-Z]+[a-zA-Z0-9_]*\\)\\>"
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-matrix-name-face t)
	  '(3 ado-matrix-name-face t) '(4 ado-matrix-name-face t))
	  ;; now for the svmat command
	(list
	  (concat
	   "\\<\\(svmat\\)\\>"
	   "[ \t]+"
	   "\\<\\([a-zA-Z]+[a-zA-Z0-9_]*\\)\\>"
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-matrix-name-face t))

	(list
	  (concat
	   "\\<\\(svmat\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"byte" "double" "float" "int" "long"
			) 'words))
	   "[ \t]+"
	   "\\<\\([a-zA-Z]+[a-zA-Z0-9_]*\\)\\>"
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	  '(3 ado-matrix-name-face t))

     ;; the ml commands
	(list
	  (concat
	   "\\<\\(ml\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			  "check" "clear" "count"
			  "di" "dis" "disp" "displ" "displa" "display"
			  "foot" "footn" "footno" "footnot" "footnote"
			  "gr" "gra" "grap" "graph"
			  "init"
			  "max" "maxi" "maxim" "maximi" "maximiz" "maximize"
			  "me" "met" "meth" "metho" "method"
			  "mod" "mode" "model"
			  "p" "pl" "plo" "plot" 
			  "q" "qu" "que" "quer" "query"
			  "rep" "repo" "repor" "report"
			  "sea" "sear" "searc" "search"
			  "trace"		   
			  ) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))
	(list
	  (concat
	   "\\<\\(ml\\)\\>"
	   "[ \t]+"
	   "\\<\\(score\\)\\>"
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))
     ;; obsolete ml commands
	(list
	  (concat
	   "\\<\\(ml\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"b" "be" "beg" "begi" "begin" 
			  "de" "dep" "depn" "depna" "depnam" "depname" "depnames" 
			  "f" "fu" "fun" "func" "funct" "functi" "functio" "function"
			  "ml" "mlo" "mlou" "mlout"
			  "pl" "plo" "plot"
			  "po" "pos" "post" 
			  "sa" "sam" "samp" "sampl" "sample"
			  ) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-obsolete-face))
	  ;; the net commands
	(list
	  (concat
	   "\\<\\(net\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			  "cd"
			  "d" "de" "des" "desc" "descr" "descri" "describ" "describe"
			  "from" "get" 
			  "ins" "inst" "insta" "instal" "install" 
			  "link"
			  "q" "qu" "que" "quer" "query"
			  "search" "sj" "stb"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

	(list
	  (concat
	   "\\<\\(net\\)\\>"
	   "[ \t]+"
	   "\\<\\(set\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"ado" "other"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	  '(3 ado-subcommand-face))
	;; ado commands
	(list
	  (concat
	   "\\<\\(ado\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"d" "de" "des" "desc" "descr" "descri" "describ" "describe"
			"dir"
			"uninstall"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

	  ;; odbc commands 
	(list
	  (concat
	   "\\<\\(odbc\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "des" "desc" "descr" "descri" "describ" "describe"
		  "li" "lis" "list"
		  "q" "qu" "que" "quer" "query"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))
	(list
	  (concat
	   "\\<\\(odbc\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "exe" "exec" 
		  "in" "ins" "inse" "inser" "insert" 
		  "lo" "loa" "load" 
		  "sql" "sqlf" "sqlfi" "sqlfil" "sqlfile" 
			) 'words))
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face))

	  ;; palette commands
	(list
	 (concat
	  "\\<\\(palette\\)\\>"
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "color"
		   "line" "linep" "linepa" "linepal" "linepale" "linepalet" "linepalett" "linepalette" 
		   "symbol" "symbolp" "symbolpa" "symbolpal" "symbolpale" "symbolpalet" "symbolpalett" "symbolpalette" 
		   ) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

	;; postutil commands - both of them
	(list
	 (concat
	  "\\<\\(postutil\\)\\>"
	  "[ \t]+"
	  "\\<\\(dir\\)\\>"
	  )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))
	(list
	 (concat
	  "\\<\\(postutil\\)\\>"
	  "[ \t]+"
	  "\\<\\(clear\\)\\>"
	  )
	 '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

	  ;; query commands 
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "q" "qu" "que" "quer" "query"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "eff" "effi" "effic" "effici" "efficie" "efficien" "efficienc" "efficiency" 
		  "graph" "graphi" "graphic" "graphics" 
		  "inter" "interf" "interfa" "interfac" "interface"
		  "mata"
		  "mem" "memo" "memor" "memory" 
		  "net" "netw" "netwo" "networ" "network" 
		  "out" "outp" "outpu" "output" 
		  "oth" "othe" "other" 
		  "trace"
		  "up" "upd" "upda" "updat" "update" 
			"paste-min-to-max stuff here"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))
    
	  ;; the reshape commands
	(list
	  (concat
	   "\\<\\(reshape\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "clear"
		  "error"
		  "i" "j"
		  "long"
		  "wide"
		  "xi" "xij"
			) 'words))
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face))
	(list
	  (concat
	   "\\<\\(reshape\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"q" "qu" "que" "quer" "query" 
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

	  ;; the snapshot commands
	(list
	  (concat
	   "\\<\\(snapshot\\)\\>"
	   "[ \t]+"
	   "\\<\\(restore\\)\\>"
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face))
	(list
	  (concat
	   "\\<\\(snapshot\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			 "erase"
			 "label" "list"
			 "save"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))
	  ;; one lonely sysuse subcommand
	(list
	  (concat
	   "\\<\\(sysuse\\)\\>"
	   "[ \t]+"
	   "\\<\\(dir\\)\\>"
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

	  ;; the _return commands (not the return commands)
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "_ret" "_retu" "_retur" "_return"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "dir" "drop"
		  "hold"
		  "res" "rest" "resto" "restor" "restore" 
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

	  ;; the return commands
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "ret" "retu" "retur" "return"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			  "add" "clear"
			  "li" "lis" "list" 
			  "loc" "loca" "local" 
			  "mat" "matr" "matri" "matrix" 
			  "sca" "scal" "scala" "scalar" 
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))
	;; ereturn
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "eret" "eretu" "eretur" "ereturn"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "clear"
		  "di" "dis" "disp" "displ" "displa" "display" 
		  "li" "lis" "list" 
		  "loc" "loca" "local" 
		  "mat" "matr" "matri" "matrix" 
		  "post" "repost"
		  "sca" "scal" "scala" "scalar" 
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))
	;; sreturn
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "sret" "sretu" "sretur" "sreturn"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		"clear" 
		  "li" "lis" "list" 
		  "loc" "loca" "local"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

	  ;; scc commands
	(list
	  (concat
	   "\\<\\(ssc\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "copy"
		  "d" "de" "des" "desc" "descr" "descri" "describ" "describe"
		  "hot"
		  "inst" "insta" "instal" "install"
		  "new" "type" "uninstall"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))
	  ;; scc commands---obsolete

	(list
	  (concat
	   "\\<\\(ssc\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "what" "whats" "whatsn" "whatsne" "whatsnew" 
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-obsolete-face))
	  ;; the serset commands
	(list
	  (concat
	   "\\<\\(serset\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "clear"
		  "cr" "cre" "crea" "creat" "create" "create_cspline" "create_xmedians"
		  "drop"
		  "reset_id"
		  "set" "sort"
		  "use"
			) 'words))
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face))
	(list
	  (concat
	   "\\<\\(serset\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "dir"
		  "su" "sum" "summ" "summa" "summar" "summari" "summariz" "summarize" 
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

	(list
	  (concat
	   "\\<\\(sts\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "g" "gr" "gra" "grap" "graph"
		  "l" "li" "lis" "list"
		  "t" "te" "tes" "test"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))
	(list
	  (concat
	   "\\<\\(sts\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			  "gen" "gene" "gener" "genera" "generat" "generate"
			) 'words))
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face))

	  ;; the sw commands
	  ;; the sw commands are all now obsolete, because of syntax change
	(list
	  (concat
	   "\\<\\(sw\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "clogit" "cloglog" "cnreg" "cox" 
		  "ereg" 
		  "gamma" "glm" "gompertz" 
		  "hetprob" 
		  "llogistic" "lnormal" "logistic" "logit" "nbreg" "ologit" "oprobit"
		  "poisson" "probit" "qreg" "reg" "regr" "regre" "regres" "regress"
		  "scobit" "stcox" "streg" "tobit" "weibull"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-obsolete-face))

	  ;; mfp arguments --- obsolete in Stata 11
	(list
	  (concat
	   "\\<\\(mfp\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			  "clogit" "cnreg" "glm" "logistic" "logit" "mlogit"
			  "nbreg" "ologit" "oprobit" "poisson"
			  "probit" "qreg" "regress" "stcox" "streg" "xtgee"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-obsolete-face))

	  ;; the sysdir commands
	(list
	  (concat
	   "\\<\\(sysdir\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "l" "li" "lis" "list"
		  "set"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	(list
	  (concat
	   "\\<\\(personal\\)\\>"
	   "[ \t]+"
	   "\\<\\(dir\\)\\>"
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	  ;; tsunab and unab commands
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "fvunab" "tsunab" "unab"
		 ) 'words))
	   "[ \t]+"
	   "\\([a-zA-Z]+[a-zA-Z0-9_]*\\)"
	   "[ \t]*:[ \t]*"
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-variable-name-face))

	  ;; the tssmooth commands
	(list
	  (concat
	   "\\<\\(tssmooth\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "d" "de" "dex" "dexp" "dexpo" "dexpon" "dexpone" "dexponen" "dexponent" "dexponenti" "dexponentia" "dexponential" 
		  "e" "ex" "exp" "expo" "expon" "expone" "exponen" "exponent" "exponenti" "exponentia" "exponential" 
		  "h" "hw" "hwi" "hwin" "hwint" "hwinte" "hwinter" "hwinters" 
		  "ma" "nl"
		  "s" "sh" "shw" "shwi" "shwin" "shwint" "shwinte" "shwinter" "shwinters" 
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))
	  ;;
	  ;; the translator commands
	(list
	  (concat
	   "\\<\\(translator\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "q" "qu" "que" "quer" "query" 
		  "reset"
		  "set"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))
	  ;;
	  ;; the transmap commands
	(list
	  (concat
	   "\\<\\(transmap\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "def" "defi" "defin" "define" 
		  "q" "qu" "que" "quer" "query" 
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))
     ;; 
     ;; the update commands
	(list
	  (concat
	   "\\<\\(update\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "ado" "all"
		  "executable"
		  "from"
		  "swap"
		  "utilities"
			) 'words))
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face))
	(list
	  (concat
	   "\\<\\(update\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "q" "qu" "que" "quer" "query" 
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

	  ;; the xtunitroot commands
	(list
	  (concat
	   "\\<\\(xtunitroot\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			 "breitung"
			 "fisher"
			 "hadri"
			 "ht"
			 "ips"
			 "llc"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

     ;; the fcast commands which leave data alone
	(list
	  (concat
	   "\\<\\(fcast\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "g" "gr" "gra" "grap" "graph" 
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))
     ;; fcast commands which alter data
	(list
	  (concat
	   "\\<\\(fcast\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "c" "co" "com" "comp" "compu" "comput" "compute" 
			) 'words))
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face))

     ;; the obsolete varfcast commands with sub commands
	(list
	  (concat
	   "\\<\\(varfcast\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "c" "cl" "cle" "clea" "clear" 
		  "co" "com" "comp" "compu" "comput" "compute" 
		  "g" "gr" "gra" "grap" "graph" 
		  "c" "co" "com" "comp" "compu" "comput" "compute" 
			) 'words))
	   )
	  '(1 ado-obsolete-face) '(2 ado-obsolete-face))

	(list
	  (concat
	   "\\<\\(irf\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "cg" "cgr" "cgra" "cgrap" "cgraph" 
		  "ct" "cta" "ctab" "ctabl" "ctable" 
		  "d" "de" "des" "desc" "descr" "descri" "describ" "describe" 
		  "di" "dir"
		  "g" "gr" "gra" "grap" "graph" 
		  "og" "ogr" "ogra" "ograp" "ograph" 
		  "t" "ta" "tab" "tabl" "table" 
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))
	 ;; irf graph/table and their subcommands
	(list
	  (concat
	   "\\<\\(irf\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"g" "gr" "gra" "grap" "graph"
		   "t" "ta" "tab" "tabl" "table"
			) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"cdm" "cirf" "coirf"
		   "dm"
		   "fevd"
		   "irf"
		   "oirf"
		   "sfevd" "sirf"
		   ) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face)
	  '(3 ado-subcommand-face t))
     ;; the irf commands which alter data
	(list
	  (concat
	   "\\<\\(irf\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "a" "ad" "add" 
		  "cr" "cre" "crea" "creat" "create" 
		  "drop"
		  "ren" "rena" "renam" "rename" 
		  "set"
			) 'words))
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face))
     ;; the irf functions which are obsolete
	(list
	  (concat
	   "\\<\\(irf\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "di" "dir"
		  "erase"
			) 'words))
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-obsolete-face t))

     ;; obsolete varirf functions
     ;; the varirf functions which leave data alone
	(list
	  (concat
	   "\\<\\(varirf\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "a" "ad" "add" 
		  "cg" "cgr" "cgra" "cgrap" "cgraph" 
		  "cr" "cre" "crea" "creat" "create" 
		  "ct" "cta" "ctab" "ctabl" "ctable" 
		  "d" "de" "des" "desc" "descr" "descri" "describ" "describe" 
		  "di" "dir"
		  "drop" "erase"
		  "g" "gr" "gra" "grap" "graph" 
		  "og" "ogr" "ogra" "ograp" "ograph" 
		  "ren" "rena" "renam" "rename" 
		  "set"
		  "t" "ta" "tab" "tabl" "table" 
			) 'words))
	   )
	  '(1 ado-obsolete-face) '(2 ado-obsolete-face t))
     ;;
     ;; the view commands
	(list
	  (concat
	   "\\<\\(view\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "ado" "ado_d"
		  "browse"
		  "file"
		  "help" "help_d"
		  "net" "net_d" "news"
		  "search" "search_d"
		  "view_d"
		  "update" "update_d"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

     ;; the webuse commands
	(list
	  (concat
	   "\\<\\(webuse\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "query" "set"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

	  ;; the window commands
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "win" "wind" "windo" "window"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "d"
		  "di" "dia" "dial" "dialo" "dialog"
		  "dir" "drop"
		  "fo" "fop" "fope" "fopen"
		  "fs" "fsa" "fsav" "fsave"
		  "l" "list"
		  "push"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "win" "wind" "windo" "window"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "c" "co" "con" "cont" "contr" "contro" "control"
			) 'words))
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "button" "check" "clear"
		  "edit"
		  "mcombo" "msimple"
		  "radbegin"
		  "radend"
		  "radio"
		  "scombo"
		  "ssimple"
		  "static"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	  '(3 ado-subcommand-face))
	;; 
	  ;; the window manage commands	
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "win" "wind" "windo" "window"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "man" "mana" "manag" "manage"
			) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "associate"
		  "maintitle"
		  "minimize" 
		  "restore"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	  '(3 ado-subcommand-face))

   ;; fix up all the 4-word crap
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "win" "wind" "windo" "window"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "man" "mana" "manag" "manage"
			) 'words))
	   "[ \t]+"
	   "\\<\\(close\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"graph" "viewer"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	  '(3 ado-subcommand-face) '(4 ado-subcommand-face))

	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "win" "wind" "windo" "window"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "man" "mana" "manag" "manage"
			) 'words))
	   "[ \t]+"
	   "\\<\\(prefs\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"default" "load" "save"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	  '(3 ado-subcommand-face) '(4 ado-subcommand-face))
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "win" "wind" "windo" "window"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "man" "mana" "manag" "manage"
			) 'words))
	   "[ \t]+"
	   "\\<\\(print\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"graph" "viewer"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	  '(3 ado-subcommand-face) '(4 ado-subcommand-face))
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "win" "wind" "windo" "window"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "man" "mana" "manag" "manage"
			) 'words))
	   "[ \t]+"
	   "\\<\\(rename\\)\\>"
	   "[ \t]+"
	   "\\<\\(graph\\)\\>"
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	  '(3 ado-subcommand-face) '(4 ado-subcommand-face))
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "win" "wind" "windo" "window"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "man" "mana" "manag" "manage"
			) 'words))
	   "[ \t]+"
	   "\\<\\(update\\)\\>"
	   "[ \t]+"
	   "\\<\\(variable\\)\\>"
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	  '(3 ado-subcommand-face) '(4 ado-subcommand-face))
	  ;; platform-specific windows manage
	  ;; need better way to work with platform-specific commands
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "win" "wind" "windo" "window"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "man" "mana" "manag" "manage"
			) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "associate"
		  "maintitle"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	  '(3 ado-platform-specific-face t))
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "win" "wind" "windo" "window"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "man" "mana" "manag" "manage"
			) 'words))
	   "[ \t]+"
	   "\\<\\(maintitle\\)\\>"
	   "[ \t]+"
	   "\\<\\(reset\\)\\>"
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	  '(3 ado-platform-specific-face t) '(4 ado-platform-specific-face t))
	  ;; the window manage forward commands [sheesh]
	;; not platform specific
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "win" "wind" "windo" "window"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "man" "mana" "manag" "manage"
			) 'words))
	   "[ \t]+"
	   "\\<\\(forward\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"command" "doeditor" "graph" "help" "results" "review" "variables" "viewer"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	  '(3 ado-subcommand-face) '(4 ado-subcommand-face))
	  ;; the window manage close/print commands [all taken care of above]

	  ;; the window menu commands
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "win" "wind" "windo" "window"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "m" "me" "men" "menu"
			) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "add_recentfiles"
		  "clear"
		  "refresh"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	  '(3 ado-subcommand-face))

	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "win" "wind" "windo" "window"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "m" "me" "men" "menu"
			) 'words))
	   "[ \t]+"
	   "\\<\\(append\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"item"
			"separator"
			"submenu"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	  '(3 ado-subcommand-face) '(4 ado-subcommand-face))

	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "win" "wind" "windo" "window"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "stop" "stopb" "stopbo" "stopbox"
			) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "note"
		  "rusure"
		  "stop"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	  '(3 ado-subcommand-face))

	  ;; the xwindow commands---obsolete from at least Stata 9.1
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "xwin" "xwind" "xwindo" "xwindow"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "de" "def" "defi" "defin" "define" 
		  "di" "dir"
		  "drop"
		  "l" "li" "lis" "list"
			) 'words))
	   )
	  '(1 ado-obsolete-face) '(2 ado-obsolete-face))

	  ;; all the endless Stata keywords (not in a good order)
	  ;; first those keywords which must start a line
	  ;; note that these will look like text if preceded by a comment
	  ;; (but comments shouldn't be before the command, anyway, 'cuz it makes things hard to read)
	(list
	 (concat
	  "^[ \t]*"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		  "by"
		  "cap" "capt" "captu" "captur" "capture"
		  "char" 
		  "e" "err" "erro" "error" "ex" "exi" "exit" 
		  "for"
		  "set"
		  ) 'words))
	  )
	 '(1 ado-builtin-harmless-face))

	  ;; here are some keywords which appear in the middle of lines
	  ;; note that the really short abbreviations could make a mess of things
	  ;;
	  ;; These are split to allow compiling!
	(list
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "_coef_table" "_crcswxx"
		  "_datasig" "_datasign" "_datasigna" "_datasignat" "_datasignatu" "_datasignatur" "_datasignature" 
		  "_qreg" "_rmcoll" "_rmdcoll" "_robust"
		  "#r" "#re" "#rev" "#revi" "#revie" "#review" 
		  "about" "ac" "acprplot" 
		  "ado" "adopath" "adoupdate" "alpha" "ameans" 
		  "an" "ano" "anov" "anova" 
		  "arch" "areg" "arima" 
		  "as" 
		  "asclogit"
		  "asmprobit"
		  "asroprobit"
		  "ass" "asse" "asser" "assert" 
		  "avplot" "avplots"
		  "b" "be" "bee" "beep"
		  "binreg" "biprobit" "biplot" "bitest" "bitesti" "blogit"
		  "bootstrap" "boxcox" "bprobit" "br" "break" "brier" 
		  "bro" "brow" "brows" "browse" 
		  "brr" "bsqreg" "bstat"
		  "ca" "cabiplot" "camat" "candisc" "canon" "caprojection" "cat" 
		  "cc" "cci" "cchart" "centile" "cf" 
		  "ch" "changeeol" "che"
		  "checkestimationsample" "checksum" 
		  "chel" "chelp"
		  "ci" "cii" 
		  "clog" "clogi" "clogit" "clogitp" "cloglog"
		  "close" "cluster" "clustermat" "cmdlog" "cmdtool" 
		  "cnsreg" "codebook" "compare" 
		  "cons" "const" "constr" "constra" "constrai" "constrain" "constraint"
		  "continue"
		  "copy" "copyright" 
		  "cor" "corc" "corr" "corre" "correl" "correla" "correlat" "correlate"
		  "corrgram"
		  "cou" "coun" "count" 
		  "cox"	"cprplot"  "cs" "csi" 
		  "ct" "ctset" 
		  "cumsp" "cumul" "cusum"
		 "command"
		 ) 'words))
	  '(1 ado-builtin-harmless-face))
	(list
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "d"
		  "datasig" "datasign" "datasigna" "datasignat" "datasignatu" "datasignatur" "datasignature"
		  "db"
		  "de" "des" "desc" "descr" "descri" "describ" "describe"
		  "dfactor" "dfbeta" "dfgls" "dfuller" 
		  "di" "dir" "dis" "discrim" "disp" "disp_res" "disp_s" 
		  "displ" "displa" "display"
		  "do" 
		  "doed" "doedi" "doedit" 
		  "dotplot"
		  "dstdize" "dvech"
		  "eivreg" "eq" "exlogistic" "expoisson"
		  "fac" "fact" "facto" "factor" "factormat"
		  "findfile" "findit" "fit"
		  "fl" "fli" "flis" "flist"
		  "for" "fpredict" 
		  "fracplot" "fracpoly" "frontier" "fsl" "fvexpand"
		  "gladder" "gllamm" "glm" "glmpred" "glogit" 
		  "gmm" "gnbreg"
		  "gphdot" "gphpen" "gprobit" "gr7" "graph7" "grmeanby"
		  "h"
		  "hadimvo" "hausman" "heckman" "heckprob" 
		  "he" "hel" "help" 
		  "hetprob" "hexdump" "hilite"
		  "hist" "histo" "histog" "histogr" "histogra" "histogram" 
		  "hlu" "hotel" "hotelling" "hsearch"
		  "include" "ins" "insp" "inspe" "inspec" "inspect" "intreg" 
		  "iqreg" "ir" "irf" "iri" 
		  "isid" "istdize" 
		  "ivprobit" "ivregress" "ivtobit"
		  "jackknife"
		  "kap" "kappa" "kapwgt" "kdensity" "ksm" "ksmirnov" "ktau"
		  "kwallis"
		 ) 'words))
	  '(1 ado-builtin-harmless-face))
	;; an experiment
	 (list
	  (concat
	   "^\\(.*:\\)*[ \t]*"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		  "l"
		  "labelbook" "ladder"
		  "levelsof"
		  "li" "line"
		  "lincom" "linktest" 
		  "lis" "list"
		  "lo" "loadingplot" "log"
		  "logi" "logistic" "logit" 
		  "loneway" "lookfor" "lowess" "lpredict" "lpoly"
		  "lroc" "lrtest" "ls" "lsens" "ltable" "lv" "lvr2plot"
		  "man" "mano" "manov" "manova" "manovatest" 
		  "margins" "matlist"
		  "mca" "mcaplot" "mcaprojection" "mcc" "mcci" 
		  "mds" "mdsconfig" "mdslong" "mdsmat" "mdsshepard"
		  "mean" "median" "memory" "mfp" "mhodds"
		  "mlog" "mlogi" "mlogit"
		  "mor" "more"
		  "mprobit" "mvreg" "mx_param"
		  "n" "nbreg" "nestreg" "net" "newey" "news"
		  "nl" "nlcom" "nlogit" "nlogittree" "nlsur" 
		  "no" "noi" "nois" "noisi" "noisil" "noisily"
		  "note" "notes" "novarabbrev"
		  "numlabel"
		  "nptrend" "numlist"
		  "olog" "ologi" "ologit"
		  "ologitp" 
		  "on" "one" "onew" "onewa" "oneway"
		  "oprob" "oprobi" "oprobit"
		  "oprobitp"
		  "pac" "pca" "pcamat" "pchart" "pchi" "pcorr" "pergram" "permute" "personal"
		  "pkcross" "pkequiv" "pkexamine" "pksumm"
		  "pl" "plo" "plot"
		  "pnorm" "poisson" "pperron"
		  "prais" "print"
		  "prob" "probi" "probit"
		  "procoverlay" "procrustes" "proportion"
		  "prtest" "prtesti"
		  "pwcorr" "pwd"
		  "q" "qchi" "qnorm" "qqplot" "qreg" "qladder" "quadchk" "quantile" 
		  "qu" "que" "quer" "query"
		  "qui" "quie" "quiet" "quietl" "quietly"
		  "ranksum" "ratio" "rchart" "regdw" "regph" 
		  "reg" "reg3" "regr" "regre" "regres" "regress"
		  "robvar"
		  "roccomp" "rocfit" "rocgold" "rocplot" "roctab"
		  "rologit" "rolling"
		  "rot" "rota" "rotat" "rotate"
		  "rotatemat"
		  "rreg"
		  "ru" "run" "runtest" "rvfplot" "rvpplot"
		 ) 'words))
	  )
	  '(2 ado-builtin-harmless-face))

	(list
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "sampsi" "sconfirm" 
		  "sc" "sca" "scat" "scatt" "scatte" "scatter" 
		  "scobit" "scoreplot" "screeplot"
		  "sdr" "sdtest" "sdtesti" "search" "serrbar" "serset"
		  "set_defaults"
		  "sfrancia" 
		  "sh" "she" "shewhart" "shel" "shell" 
		  "signestimationsample" "signrank" "signtest"
		  "sktest" "sleep" "slog" "slogit" "spearman" "spikeplot" "sqreg"
		  "ssc" "sspace"
		  "st" "st_is" "st_show" "st_ct" "stci"
		  "stcox" "stcoxkm" 
		  "stcrr" "stcrre" "stcrreg"
		  "stcurv" "stcurve" "stdescribe"
		  "stem" "stepwise"
		  "stereg" "stir" "stmc" "stmh" "stphplot" "stptime" 
		  "strate" "streg" "streset"
		  "sts" "stse" "stset" "stsum" "stvary" "stweib"
		  "su" "suest" "sum" "summ" "summa" "summar" "summari" "summariz" "summarize"
		  "sureg" "sunflower" "svar"
		  "svydes" "svydescribe" "svyset"
		  "sw" "swilk" "symmetry" "symmi" "symplot" "syntax" "sysdir" 
		  "ta" "tab" 
		  "tab1" "tab2" 
		  "tabdisp"
		  "tabi" 
		  "table" "tabodds" "tabstat"
		  "tabu" "tabul" "tabula" "tabulat" "tabulate"
		  "te" "tes" "test"
		  "testnl" "testparm" "tetrachoric"
		  "tnbreg"
		  "tob" "tobi" "tobit"
		  "token" "tokeni" "tokeniz" "tokenize" 
		  "total" "touch" 
		  "tpoisson"
		  "translator" "transmap" "treatreg" "truncreg"
		  "tsline" "tsreport" "tsrline" "tsset" "tssmooth" "tsunab" "ttest" "ttesti"
		  "twoway"
		  "ty" "typ" "type"
		  "unab" "unabcmd" "update" "using"
		  "var" "varabbrev" "varbasic" "vargranger"  
		  "varlmar" 
		  "varm" "varma" "varman" "varmana" "varmanag" "varmanage" "varmanager" 
		  "varnorm" "varsoc" "varstable" "varwle" 
		  "vec" "veclmar" "vecnorm" "vecrank" "vecstable"
		  "verinst" "view" "viewsource" "vwls"
		  "which" "who" "wntestb" "wntestq" 
		  "xchart" "xcorr"
		  "xsh" "xshe" "xshel" "xshell" 
		  "xtabond" "xtclog" "xtcloglog" 
		  "xtdes" "xtdesc" "xtdescr" "xtdescri" "xtdescrib" "xtdescribe"
		  "xtdpd" "xtdpdsys"
		  "xtfrontier"
		  "xtgee" "xtgls" "xthtaylor" "xtintreg" "xtivreg"
		  "xtline" "xtlogit" "xtmelogit" "xtmepoisson" "xtmixed" 
		  "xtnbreg" "xtpcse" "xtpois" "xtpoisson" "xtprobit"
		  "xtrc" "xtreg" "xtregar" "xtset" "xtsum" "xttab" "xttest0" "xttobit" "xttrans"
		  "zinb" "zip" "ztnb" "ztb"
		 ) 'words))
	  '(1 ado-builtin-harmless-face))

	  ;; things to use with the svy ... : prefix
	  ;; can be fooled by svy brr, nothing but garbage:
	  ;; another problem: don't know how to make the hilighted stuff in the middle
	  ;;  optional
	(list
	  (concat
	   "\\<\\(svy\\)\\>"
	   "[ \t]*,?.*?:[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"biprobit"
			"clogit" "cloglog" "cnreg" "cnsreg"
			"glm" "gnbreg" 
			"heckman" "heckprob" "hetprob" 
			"intreg" "ivprobit" "ivregress" "ivtobit"
			"logistic" "logit"
			"mean" "mprobit" "mlogit"
			"nl"
			"nbreg" "ologit" "oprobit"
			"poisson" "probit" "proportion" 
			"ratio" 
			"reg" "regr" "regre" "regres" "regress"
			"scobit" "slogit" "stcox" "streg"
			"tab" "tabu" "tabul" "tabula" "tabulat" "tabulate" 
			"tobit" "total" "treatreg" "truncreg"
			"zinb" "zip" "ztnb" "ztb"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face))
	;; more svy stuff
	(list
	  (concat
	   "\\<\\(svy\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"bootstrap"
			"brr"
			"jack" "jackk" "jackkn" "jackkni" "jackknif" "jackknife" 
			"linear" "lineari" "lineariz" "linearize" "linearized" 
			"sdr"
			) 'words))
	   ".*?,?.*?:[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "gnbreg" 
		  "heckman" "heckprob" 
		  "intreg" "ivreg" 
		  "logistic" "logit"
		  "mean" "mlogit" 
		  "nbreg" "ologit" "oprobit"
		  "poisson" "probit" "proportion" 
		  "ratio" 
		  "reg" "regr" "regre" "regres" "regress" 
		  "tab" "tabu" "tabul" "tabula" "tabulat" "tabulate" 
		  "total"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	  '(3 ado-builtin-harmless-face))

	  ;; haver subcommands
	(list
	  (concat
	   "\\<\\(haver\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"des" "desc" "descr" "descri" "describ" "describe"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face))
	(list
	  (concat
	   "\\<\\(haver\\)\\>"
	   "[ \t]+"
	   "\\<\\(use\\)\\>"
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-builtin-harmful-face))

	  ;; Conditional statements 
	  ;; if might not work right ('cuz it is also a keyword)
	(list
	 (concat
	  (eval-when-compile
		(regexp-opt
		 '(
		   "if" "else" "while"
		   ) 'words ))
	  "[ \t]+.*?{"
	  )
	 '(1 ado-builtin-harmless-face t))

	  ;; variable types which appear as subcommands often
	(list
	 (eval-when-compile 
	   (regexp-opt 
		'(
		  "byte" "int" "long" "str" "float" "double"
		  ) 'words))
	 '(1 ado-subcommand-face))

	;; string variable types
	(list
	 (concat
	  "\\<\\(str"
	  "\\(?:2\\(?:[0-3][0-9]\\|4[0-4]\\)"
	  "\\|"
	  "1[0-9][0-9]"
	  "\\|"
	  "[1-9][0-9]?"
	  "\\)\\)\\>"
	  )
	 '(1 ado-subcommand-face))

	  ;; things used with display
	  ;; since these are often split across lines, and Stata commands are hard
	  ;; to delimit, this will highlight even if out of context

	  ;;
	  ;; plain display
	  ;;
		(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "di" "dis" "disp" "displ" "displa" "display"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "_asis"
		  "_c" "_co" "_con" "_cont" "_conti" "_contin" "_continu" "_continue"
		  "_n" "_ne" "_new" "_newl" "_newli" "_newlin" "_newline" 
		  "_quote"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "di" "dis" "disp" "displ" "displa" "display"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"_r" "_re" "_req" "_requ" "_reque" "_reques" "_request"
			) 'words))
	   "([ \t]*"
	   "\\([a-zA-Z]+[a-zA-Z0-9_]*\\)"
	   "[ \t]*)"
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face)
	  '(3 ado-variable-name-face))
	  ;; display things which take numbers
	  ;; some left as is, because they are used in dictionaries...
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "di" "dis" "disp" "displ" "displa" "display"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "_char"
		  "_col" "_colu" "_colum" "_column"
		  "_d" "_du" "_dup"
		  "_n" "_ne" "_new" "_newl" "_newli" "_newlin" "_newline"
		  "_s" "_sk" "_ski" "_skip"
			) 'words))
	   "([ \t]*"
	   "\\([1-9]+[0-9]*\\)"
	   "[ \t]*)"
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face)
	  '(3 ado-constant-face))
	  ;;
	  ;; other display subcommands
	(list
	 (concat
	  (eval-when-compile 
		(regexp-opt 
       '(
		 "di" "dis" "disp" "displ" "displa" "display"
		 ) 'words))
	   "[ \t]+"
	   "\\<\\(as\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "err" "erro" "error" 
		  "inp" "inpu" "input"
		  "res" "resu" "resul" "result" 
		  "text" "txt"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	  '(3 ado-subcommand-face))

	;; trust Stata corp to use different prepositions...
	(list
	 (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "di" "dis" "disp" "displ" "displa" "display"
		 ) 'words))
	   "[ \t]+"
	   "\\<\\(in\\)\\>"
	   "[ \t]+"
	   "\\<\\(smcl\\)\\>"
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face)
	  '(3 ado-subcommand-face))

     ;;
     ;; obsolete coloring
	(list
	 (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "di" "dis" "disp" "displ" "displa" "display"
		 ) 'words))
	   "[ \t]+"
	   "\\<\\(in\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "b" "bl" "blu" "blue" 
		  "g" "gr" "gre" "gree" "green"
		  "r" "re" "red"
		  "w" "wh" "whi" "whit" "white"
		  "y" "ye" "yel" "yell" "yello" "yellow"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-obsolete-face)
	  '(3 ado-obsolete-face))
     ;;
     ;; foreach ... in
	(list
	 (concat
	  "\\<\\(foreach\\)\\>"
	   "[ \t]+"
	   "\\([a-zA-Z]+[a-zA-Z0-9_]*\\)"
	   "[ \t]+"
	   "\\<\\(in\\)\\>"
	   "[ \t]+"
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-variable-name-face)
	  '(3 ado-subcommand-face))

	;; foreach ... of
	(list
	 (concat
	  "\\<\\(foreach\\)\\>"
	   "[ \t]+"
	   "\\([a-zA-Z]+[a-zA-Z0-9_]*\\)"
	   "[ \t]+"
	   "\\<\\(of\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "glo" "glob" "globa" "global"
		  "loc" "loca" "local" 
		  "new" "newl" "newli" "newlis" "newlist" 
		  "num" "numl" "numli" "numlis" "numlist" 
		  "var" "varl" "varli" "varlis" "varlist" 
		 ) 'words))
	   )
	 '(1 ado-builtin-harmless-face) '(2 ado-variable-name-face t)
	 '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))

	  ;; forvalues ... = ??
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "forv" "forva" "forval" "forvalu" "forvalue" "forvalues"
		 ) 'words))
	   "[ \t]+"
	   "\\([a-zA-Z]+[a-zA-Z0-9_]*\\)"
	   "[ \t]*=[ \t]*"
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-variable-name-face t))

     ;; gettoken
	(list
	  (concat
	   "\\<\\(gettoken\\)\\>"
	   "\\(\\(?:[ \t]+(\\(?:loc\\|glob\\)al)\\)?\\)"
	   "\\([ \t]+[a-zA-Z]+[a-zA-Z0-9_]*\\)"
	   "\\(\\(?:[ \t]+(\\(?:loc\\|glob\\)al)\\)?\\)"
	   "\\([ \t]+[a-zA-Z]+[a-zA-Z0-9_]*\\)"
        "[ \t]*:[ \t]*"
		"\\([a-zA-Z]+[a-zA-Z0-9_]*\\)"
		)
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	  '(3 ado-variable-name-face t) '(4 ado-subcommand-face t)
	  '(5 ado-variable-name-face t) '(6 ado-variable-name-face t))

	(list
	  (concat
	   "\\<\\(gettoken\\)\\>"
	   "\\(\\(?:[ \t]+(\\(?:loc\\|glob\\)al)\\)?\\)"
	   "\\(\\(?:[ \t]+[a-zA-Z]+[a-zA-Z0-9_]*\\)\\{1,2\\}\\)"
        "[ \t]*:[ \t]*"
		"\\([a-zA-Z]+[a-zA-Z0-9_]*\\)"
		)
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	  '(3 ado-variable-name-face t) '(4 ado-variable-name-face t))

	  ;; labels
	(list
	 (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "la" "lab" "labe" "label"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			 "copy"
			 "da" "dat" "data"
			 "de" "def" "defi" "defin" "define"
			 "di" "dir" 
			 "drop" 
			 "lang" "langu" "langua" "languag" "language" 
			 "l" "li" "lis" "list"
			 "save"
			 "val" "valu" "value" "values"
			 "var" "vari" "varia" "variab" "variabl" "variable"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t))

	  ;; all Stata data-altering stuff
		(list
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "_pctile" "_predict"
		  "ap" "app" "appe" "appen" "append" 
		  "bcskew0" "bs" "bsample" "bstrap"
		  "bys" "byso" "bysor" "bysort" 
		  "cd" "clear" "clonevar" "collapse" "compress" 
		  "contract" "convert" "corr2data" "cross" "cttost" 
		  "dec" "deco" "decod" "decode" "destring"
		  "discard" "drawnorm" "drop" "dydx"
		  "ed" "edi" "edit" "egen" 
		  "en" "enc" "enco" "encod" "encode"
		  "erase"
		  "expand" "expandcl"
		  "fdades" "fdadesc" "fdadescr" "fdadescri" "fdadescrib" "fdadescribe" 
		  "fdasav" "fdasave" 
		  "fdause"
		  "filef" "filefi" "filefil" "filefilt" "filefilte" "filefilter" 
		  "fillin"
		  "form" "forma" "format"
		  "fracgen" "fracpred"
		  "fvrevar"
		  "g" "ge" "gen" "gene" "gener" "genera" "generat" "generate"
		  "gsort"
		  "inf" "infi" "infile" "infix" 
		  "inp" "inpu" "input"
		  "insheet" "integ" "ipolate" 
		  "joinby"
		  "keep" 
		  "lnskew0"
		 ) 'words))
	  '(1 ado-builtin-harmful-face))
	(list
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "makecns"
		  "mark" "markin" "markout"
		  "mata" "matrix"
		  "mkdir" "mkmat" "mkspline"
		  "mleval" "mlmatsum" "mlsum""mlvecsum"
		  "modify" 
		  "mvdecode" "mvencode" 
		  "nlogitgen" "nlpred" "nobreak" 
		  "order" "orthog" "orthpoly"
		  "ou" "out" "outf" "outfi" "outfil" "outfile"
		  "outs" "outsh" "outshe" "outshee" "outsheet"
		  "pctile" 
		  "pkcollapse" "pkshape"
		  "post" "postclose" "postfile" 
		  "predict" "predictnl"
		  "preserve" "range"
		  "recast" "recode" 
		  "ren" "rena" "renam" "rename"
		  "renpfix" "replace" "reshape" "restore" "rm" "rmdir"
		  "sappend" 
		  "sa" "sav" "save" "saveold"
		  "sample" "sdrop"
		  "separate"
		  "simul" "simulate" "sinfile" "smerge" 
		  "smooth" "snapspan" 
		  "so" "sor" "sort" "sortpreserve"
		  "split"
		  "ssave" "ssort" "stack" "statsby"
		  "stbase" "stfill" "stgen" "stjoin" "stsplit" "sttocc" "sttoct"
		  "suse" "svmat" "svymarkout" "sysuse"
		  "tostring"
		  "translate"
		  "tsappend" "tsfill" "tsrevar"
		  "u" "unzipfile" "us" "use" "uselabel"
		  "webuse"
		  "xi" "xi:" 
		  "xmlsav" "xmlsave" "xmluse" 
		  "xtile" "xpose" 
		  "xtdata" "xtpred"
		  "zipfile"
		 ) 'words))
	  '(1 ado-builtin-harmful-face))

	(list
	  (concat
	   "\\<\\(clear\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			  "ado" "all"
			  "mata" "matrix"
			  "programs"
			  "results"
			) 'words))
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))
			  
	  ;; assignment of macros
	  ;;  local macros have different naming conventions (boo)
	  ;;  marksample added, because need harmful/scalar name
	(list
	  (concat
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"gl" "glo" "glob" "globa" "global" 
			"marksample"
			"sca" "scal" "scala" "scalar" 
			) 'words))
	   "[ \t]+`*"
	   "\\([a-zA-Z_]+[a-zA-Z_0-9]*\\)"
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-variable-name-face t))
	;; local macro definition
	(list
	  (concat
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "loc" "loca" "local" 
			) 'words))
	   "[ \t]+\\(?:\\(?:++\\|--\\|[`]+\\)?\\)"
	   "\\([a-zA-Z_0-9]+\\)"
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-variable-name-face t))

	  ;; warning for local wrong--
	(list
	  (concat
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "loc" "loca" "local" 
			) 'words))
	   "[ \t]+"
	   "\\(\\(?:`\\|[a-zA-Z_0-9]\\|'\\)+\\)"
	   "\\(\\(?:++\\|--\\)\\)"
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-variable-name-face t)
	  '(3 ado-obsolete-face t))
	;; scalars
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "sca" "scal" "scala" "scalar"
		 ) 'words))
	   "\\(\\(?:[ \t]+\\(?:def\\|defi\\|defin\\|define\\)\\)?\\)"
	   "[ \t]+"
	   "\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	  '(3 ado-variable-name-face t))
	;; scalar/macro drop etc.
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "ma" "mac" "macr" "macro"
		 "sca" "scal" "scala" "scalar" 
		 ) 'words))
	   "[ \t]+"
	   "\\<\\(drop\\)\\>"
	   "[ \t]+"
	   "\\([a-zA-Z_]+[a-zA-Z_0-9]*\\b\\)"
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face)
	  '(3 ado-variable-name-face t))
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "ma" "mac" "macr" "macro"
		 "sca" "scal" "scala" "scalar" 
		 ) 'words))
	   "[ \t]+"
	   "\\<\\(drop\\)\\>"
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "loc" "loca" "local" 
		 ) 'words))
	   "[ \t]+"
	   "\\<\\(drop\\)\\>"
	   "[ \t]+"
	   "\\(\\(?:[a-zA-Z_0-9]+\\b\\)?\\)"
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-obsolete-face)
	  '(3 ado-variable-name-face t))

	  ;;
	  ;; an attempt to include the extended macro names
	  ;; single word extended macro names
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		 ) 'words))
	   "[ \t]+"
	   "\\([a-zA-Z_]+[a-zA-Z_0-9]*\\)"
	   "[ \t]*:[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "adosubdir"
		  "char" "cole" "coleq" 
		  "colf" "colfu" "colful" "colfull" "colfulln" "colfullna" "colfullnam" "colfullname" "colfullnames" 
		  "coln" "colna" "colnam" "colname" "colnames" 
		  "constraint"
		  "dirsep" 
		  "di" "dir" "dis" "disp" "displ" "displa" "display" 
		  "env" "envi" "envir" "enviro" "environ" "environm" "environme" "environmen" "environment" 
		  "f" "fo" "for" "form" "forma" "format" 
		  "lab" "labe" "label"
		  "list"
		  "permname" "piece" "properties" "pwd"
		  "rowe" "roweq" 
		  "rowf" "rowfu" "rowful" "rowfull" "rowfulln" "rowfullna" "rowfullnam" "rowfullname" "rowfullnames" 
		  "rown" "rowna" "rownam" "rowname" "rownames" 
		  "sort" "sorte" "sorted" "sortedb" "sortedby" 
		  "tempf" "tempfi" "tempfil" "tempfile" "tempv" "tempva" "tempvar" 
		  "tsnorm" 
		  "ty" "typ" "type"
		  ) 'words))
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	  '(3 ado-subcommand-face t))
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		 ) 'words))
	   "[ \t]+"
	   "\\([a-zA-Z_]+[a-zA-Z_0-9]*\\)"
	   "[ \t]*:[ \t]*"
	   "\\<\\(copy\\)\\>"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "global" "local"
		 ) 'words))
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	  '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		 ) 'words))
	   "[ \t]+"
	   "\\([a-zA-Z_]+[a-zA-Z_0-9]*\\)"
	   "[ \t]*:[ \t]*"
	   "\\<\\(word\\)\\>"
	   "[ \t]+"
	   "\\<\\(count\\)\\>"
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	  '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		 ) 'words))
	   "[ \t]+"
	   "\\([a-zA-Z_]+[a-zA-Z_0-9]*\\)"
	   "[ \t]*:[ \t]*"
	   "\\<\\(word\\)\\>"
	   "[ \t]+\\(?:[0-9]+\\|`[^ \t]*'\\)[ \t]+"
	   "\\<\\(of\\)\\>"
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	  '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))

     ;; things with parens in them (sheesh)
     ;; not included above, incase someone uses a font which has a background color
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		 ) 'words))
	   "[ \t]+"
	   "\\([a-zA-Z_]+[a-zA-Z_0-9]*\\)"
	   "[ \t]*:[ \t]*"
	   "\\<\\(e\\|r\\)\\>"
	   "([ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "functions" "macros" "matrices" "scalars"
		 ) 'words))
	   "[ \t]*)"
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	  '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))

     ;; damn s(macros)
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		 ) 'words))
	   "[ \t]+"
	   "\\([a-zA-Z_]+[a-zA-Z_0-9]*\\)"
	   "[ \t]*:[ \t]*"
	   "\\<\\(s\\)\\>"
	   "([ \t]*"
	   "\\<\\(macros\\)\\>"
	   "[ \t]*)"
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	  '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))
     ;; twin word macros length and subinstr
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		 ) 'words))
	   "[ \t]+"
	   "\\([a-zA-Z_]+[a-zA-Z_0-9]*\\)"
	   "[ \t]*:[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "length" "subinstr"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		 ) 'words))
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	  '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))
	  
     ;; serset macros
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		 ) 'words))
	   "[ \t]+"
	   "\\([a-zA-Z_]+[a-zA-Z_0-9]*\\)"
	   "[ \t]*:[ \t]*"
	   "\\<\\(serset\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "N"
		  "format" "k" "id" "max" "min" "type" "varnames" "varnum"
		 ) 'words))
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	  '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))
     ;; sheesh, now there are combined abbreviations!
     ;;
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		 ) 'words))
	   "[ \t]+"
	   "\\([a-zA-Z_]+[a-zA-Z_0-9]*\\)"
	   "[ \t]*:[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "data"
		  "val" "valu" "value" 
		  "var" "vari" "varia" "variab" "variabl" "variable" 
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "l" "la" "lab" "labe" "label"
		 ) 'words))
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	  '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))
	  
     ;; macro list commands start here
     ;; single word commands
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		 ) 'words))
	   "[ \t]+"
	   "\\([a-zA-Z_]+[a-zA-Z_0-9]*\\)"
	   "[ \t]*:[ \t]*"
	   "\\<\\(list\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "clean"
		  "dups"
		  "retok" "retoke" "retoken" "retokeni" "retokeniz" "retokenize"
		  "sizeof"
		  "sort"
		  "uniq"
		 ) 'words))
	   "[ \t]+"
	   "\\([a-zA-Z_]+[a-zA-Z_0-9]*\\)"
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	  '(3 ado-subcommand-face t) '(4 ado-subcommand-face t)
	  '(5 ado-variable-name-face t))
     ;; operator-like word commands

	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		 ) 'words))
	   "[ \t]+"
	   "\\([a-zA-Z_]+[a-zA-Z_0-9]*\\)"
	   "[ \t]*:[ \t]*"
	   "\\<\\(list\\)\\>"
	   "[ \t]+"
	   "\\([a-zA-Z_]+[a-zA-Z_0-9]*\\)"
	   "[ \t]*"
	   "\\(?:[|&-]\\|==\\|===\\|in\\)"
	   "[ \t]*"
	   "\\([a-zA-Z_]+[a-zA-Z_0-9]*\\)"
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	  '(3 ado-subcommand-face t) '(4 ado-variable-name-face t)
	  '(5 ado-variable-name-face t))
     ;; friggin' posof subcommand
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		 ) 'words))
	   "[ \t]+"
	   "\\([a-zA-Z_]+[a-zA-Z_0-9]*\\)"
	   "[ \t]*:[ \t]*"
	   "\\<\\(list\\)\\>"
	   "[ \t]+"
	   "\\<\\(posof\\)\\>"
	   "[ \t]+\".*?\"[ \t]+"
	   "\\<\\(in\\)\\>"
	   "[ \t]+"
	   "\\([a-zA-Z_]+[a-zA-Z_0-9]*\\)"
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	  '(3 ado-subcommand-face t) '(4 ado-subcommand-face t)
	  '(5 ado-subcommand-face t) '(6 ado-variable-name-face t))

     ;; all subcommand
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		 ) 'words))
	   "[ \t]+"
	   "\\([a-zA-Z_]+[a-zA-Z_0-9]*\\)"
	   "[ \t]*:[ \t]*"
	   "\\<\\(all\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "globals" "matrices" "scalars"
		 ) 'words))
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	  '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))
     ;; all numeric/string
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		 ) 'words))
	   "[ \t]+"
	   "\\([a-zA-Z_]+[a-zA-Z_0-9]*\\)"
	   "[ \t]*:[ \t]*"
	   "\\<\\(all\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "numeric" "string"
		 ) 'words))
	   "[ \t]+"
	   "\\<\\(scalars\\)\\>"
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	  '(3 ado-subcommand-face t) '(4 ado-subcommand-face t)
	  '(5 ado-subcommand-face t))

	 ;; obsolete macro extended commands
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		 ) 'words))
	   "[ \t]+"
	   "\\([a-zA-Z_]+[a-zA-Z_0-9]*\\)"
	   "[ \t]*:[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "tempf" "tempfi" "tempfil" "tempfile" "tempv" "tempva" "tempvar" 
		 ) 'words))
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	  '(3 ado-obsolete-face t))

	  ;; choosing temp names
	(list
	  (concat
	   "^[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "tempfile" "tempname" "tempvar"
		 ) 'words))
	   "[ \t]+`*"
	   "\\(\\(?:[a-zA-Z_]+[a-zA-Z_0-9]*[ \t]*\\)+\\)"
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-variable-name-face t))

	  ;; other macro commands
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "ma" "mac" "macr" "macro"
		 "sca" "scal" "scala" "scalar" 
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "di" "dir"
		  "l" "li" "lis" "list"			
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t))

	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "ma" "mac" "macr" "macro"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "s" "sh" "shi" "shif" "shift"
			) 'words))
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face))

	  ;; stata functions i.e. things which require () after them 
	  ;; obsolete functions are after this list
	  ;; finally included matrix functions
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "_byn1"  "_byn2" "_caller" 
			 "abbrev" "abs" "acos" "asin" "atan" "atan2" "atanh" "autocode"
			 "betaden" "binomial" "binomialp" "binomialtail" 
			 "binormal" "byteorder"
			 "Cdhms" "Chms" "Clock" "Cmdyhms" "Cofc" "Cofd"
			 "c" "ceil" "char" "chi2" "chi2tail" "cholesky" "chop" "cofC" "cofd" "comb" 
			 "clip" "cloglog" "clock" 
			 "colnumb" "colsof" "cond" "corr" "cos" 
			 "daily" "date" "day"
			 "det"
			 "dgammapda" "dgammapdada" "dgammapdadx" "dgammapdx" "dgammapdxdx"
			 "dhms"
			 "diag" "diag0cnt" "digamma" 
			 "dofC" "dofc" "dofd" "dofh" "dofm" "dofq" "dofw" "dofy" "dow" "doy"
			 "e" "el" "epsdouble" "epsfloat" "exp"
			 "F" "Fden" "Ftail" "float" "floor" "fmtwidth" 
			 "gammaden" "gammap" "gammaptail" "get"
			 "hadamard" "halfyear" "halfyearly" "has_eprop" "hh" "hhC" "hofd" "hms" "hours" "hypergeometric" "hypergeometricp"
			 "I" "ibeta" "ibetatail" "indexnot" "inlist" "inrange" "int"
			 "inv" "invbinomial" "invbinomialtail" "invchi2" "invchi2tail" "invcloglog"
			 "invF" "invFtail" "invgammap" "invgammaptail" "invibeta" "invibetatail" "invlogit"
			 "invnbinomial" "invnbinomialtail"
			 "invnchi2" "invnFtail" "invnibeta" "invnormal" 
			 "invpoisson" "invpoissontail"
			 "invsym" "invttail"
			 "irecode" "issymetric" "itrim"
			 "J"
			 "length" "ln" "lnfactorial" "lngamma" "log" "log10" "logit" "lower" "ltrim" 
			 "matmissing" "matrix" "matuniform" 
			 "max" "maxbyte" "maxdouble" "maxfloat" "maxint" "maxlong" 
			 "mdy" "mdyhms" "mi"
			 "min" "minbyte" "mindouble" "minfloat" "minint" "minlong" "minutes"
			 "missing" "mm" "mmC" "mod" "mofd" "month" "monthly" "mreldif"
			 "msofhours" "msofminutes" "msofseconds"
			 "nbetaden" "nchi2" 
			 "nbinomial" "nbinomialp" "nbinomialtail"
			 "normal" "normalden" "nFden" "nFtail" "nibeta" "npnchi2" "nullmat"
			 "poisson" "poissonp" "poissontail"
			 "plural" "proper"
			 "qofd" "quarter" "quarterly"
			 "r" "rbeta" "rbinomial" "rchi2" "real" "recode"
			 "regexm" "regexr" "regexs"
			 "reldif" "replay" "return" "reverse" 
			 "rgamma" "rhypergeometric" "rnbinomial" "rnormal"
			 "round" "rownumb" "rowsof" "rpoisson" "rt" "rtrim" "runiform"
			 "s" "scalar" "seconds" "sign" "sin" "soundex" "soundex_nara"
			 "sqrt" "ss" "ssC"
			 "string" "strlen" "strmatch" "strofreal" "strpos" "strtoname"
			 "subinstr" "subinword" "substr" "sum" 
			 "sweep" 
			 "tC"
			 "tan" "tanh" "tc" "td" "tden" "th" "tin" "tm" "tq" "trace" "trigamma" "trim" "trunc" "ttail" "tw" "twithin"
			  "upper"
			  "vec" "vecdiag"
			  "week" "weekly" "wofd" "word" "wordcount"
			  "year" "yearly" "yh" "ym" "yofd" "yq" "yw"

		 ) 'words))
	   "("
	   )
	  '(1 ado-function-name-face t))

	  ;;
	  ;; obsolete functions requiring () after them
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"Binomial"
			"binorm" 
			"chiprob"
			"d"
			"fprob"
			"group"
			"h" 
			"index"
			"invchi" "invfprob" "invnchi" "invnorm" "invt" 
			"issym"
			"lnfact"
			"m" "match"
			"nchi" "norm" "normd" "normden" "normprob" "npnchi"
			"q" 
			"syminv" "tprob" 
			"uniform" "uniform0"
			"y"
			"w"
			) 'words))
	   "("
	   )
	  '(1 ado-obsolete-face t))

	  ;; things which require [] after them
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "_b" "_coef" "_se"
		 ) 'words))
	   "\\["
	   )
	  '(1 ado-builtin-harmless-face))

	  ;; common Stata options which require a () after them
	;; kind of out-of-place, but left as a historical remnant
	(list
	  (concat
	   ",.*[\ t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "bands" "by" "connect" "density" "gap" "iterate" "ltolerance" "margin"
		  "psize" "saving" "tlabel" "tolerance"
		  "xlabel" "xscale" "ylabel" "yscale"
		 ) 'words))
	   )
	  '(1  ado-subcommand-face t))

	  ;; egen 'function' options
	(list
	  (concat
	   "[ \t]*egen[ \t]+.*=[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
			  "anycount" "anymatch" "anyvalue"
			  "concat" "count" "cut" 
			  "diff"
			  "ends" 
			  "fill" "group" "iqr"
			  "kurt"
			  "mad" "max" "mdev" "mean" "median" "min" "mode" "mtr" 
			  "pc" "pctile" 
			  "rank" 
			  "rowfirst" "rowlast" "rowmax" "rowmean" "rowmedian" "rowmin" "rowmiss" "rownonmiss" "rowpctile" "rowsd" "rowtotal" 
			  "sd" "seq" "skew" "std" 
			  "tag" "total"
		 ) 'words))
	   )
	  '(1 ado-function-name-face t))

	  ;; egen 'function' options -- obsolete
	(list
	  (concat
	   "[ \t]*egen[ \t]+.*=[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
			  "any"
			  "eqany"
			  "ma"
			  "neqany"
			  "rfirst" "rlast" "rmax" "rmean" "rmin" "rmiss" "robs" "rsd" "rsum" 
			  "sum"
		 ) 'words))
	   )
	  '(1 ado-obsolete-face t))

	  ;; all the endless -mi- commands
	(list
	  (concat
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
			"copy"
			"d" "de" "des" "desc" "descr" "descri" "describ" "describe"
			"est" "esti" "estim" "estima" "estimat" "estimate"
			"fvset"
			"q" "qu" "que" "quer" "query"
			"st" "streset" "stset"  "svyset"
			"test" "testtr" "testtra" "testtran" "testtrans" "testtransf" "testtransfo" "testtransfor" "testtransform"
			"tsset"
			"unreg" "unregi" "unregis" "unregist" "unregiste" "unregister"
			"unset" "update"
			"vary" "varyi" "varyin" "varying"
			"xtset"
		 ) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	  ;; mi harmful commands
	(list
	  (concat
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
			"add" "append"
			"erase" "expand" "extract"
			"ren" "rena" "renam" "rename"
			"replace0"
			"reset"
			"select" "stsplit"
		 ) 'words))
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

	  ;; mi convert commands
	(list
	  (concat
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   "\\<\\(convert\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
			"fl" "flo" "flon" "flong"
			"flongs" "flongse" "flongsep"
			"ml" "mlo" "mlon" "mlong"
			"w" "wi" "wid" "wide"
		 ) 'words))
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	  '(3 ado-subcommand-face t))

	  ;; mi export commands
	(list
	  (concat
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   "\\<\\(export\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "ice"
		 "nhanes1"
		 ) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	  '(3 ado-subcommand-face t))
	  ;; mi import commands
	(list
	  (concat
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   "\\<\\(import\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "flong" "flongsep"
		 "ice"
		 "nhanes1"
		 "wide"
		 ) 'words))
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	  '(3 ado-subcommand-face t))
	  ;; mi impute commands
	(list
	  (concat
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "imp" "impu" "imput" "impute"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
			"logi" "logit"
			"mlog" "mlogi" "mlogit"
			"mon" "mono" "monot" "monoto" "monoton" "monotone"
			"mvn"
			"olog" "ologi" "ologit"
			"pmm"
			"reg" "regr" "regre" "regres" "regress"
		 ) 'words))
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	  '(3 ado-subcommand-face t))
	  ;; mi merge commands
	(list
	  (concat
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   "\\<\\(merge\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "1:1" "1:m" "m:1" "m:m"
		 ) 'words))
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	  '(3 ado-subcommand-face t))
	  ;; mi misstable commands
	(list
	  (concat
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "misstab" "misstabl" "misstable"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
			"nest" "neste" "nested"
			"pat" "patt" "patte" "patter" "pattern" "patterns"
			"sum" "summ" "summa" "summar" "summari" "summariz" "summarize"
			"tree"
		 ) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	  '(3 ado-subcommand-face t))
	  ;; mi passive subcommands
	(list
	  (concat
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "pas" "pass" "passi" "passiv" "passive"
		 ) 'words))
	   "[ \t]*:"
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))
	  ;; mi ptrace subcommands
	(list
	  (concat
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   "\\<\\(ptrace\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
			"d" "de" "des" "desc" "descr" "descri" "describ" "describe"
			"use"
		 ) 'words))
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	  '(3 ado-subcommand-face t))
	  ;; mi reshape subcommands
	(list
	  (concat
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   "\\<\\(reshape\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "long" "wide"
		 ) 'words))
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	  '(3 ado-subcommand-face t))
	  ;; mi select subcommands
	(list
	  (concat
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   "\\<\\(select\\)\\>"
	   "[ \t]+"
	   "\\<\\(init\\)\\>"
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	  '(3 ado-subcommand-face t))
	  ;; mi set subcommands
	(list
	  (concat
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   "\\<\\(set\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
			"M"
			"fl" "flo" "flon" "flong"
			"flongs" "flongse" "flongsep"
			"m" "ml" "mlo" "mlon" "mlong" 
			"w" "wi" "wid" "wide" 
		 ) 'words))
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t)
	  '(3 ado-subcommand-face t))
	  ;; mi register subcommands
	(list
	  (concat
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "reg" "regi" "regis" "regist" "registe" "register"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
			"imp" "impu" "imput" "impute" "imputed" 
			"pas" "pass" "passi" "passiv" "passive"
			"reg" "regu" "regul" "regula" "regular"
		 ) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	  '(3 ado-subcommand-face t))
	  ;; mi xeq subcommands
	(list
	  (concat
	   "\\<\\(mi\\)\\>"
	   "[ \t]+"
	   "\\<\\(xeq\\)\\>"
	   "[ \t]*:"
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))
	;; commented out for version 1.11.1.0
	  ;; All Bill R's Custom ado files which are 'reliable' and which 
	;; are not file killers
	;; (list
	;;    (eval-when-compile 
	;; 	 (regexp-opt 
    ;;    '(
	;; 	 "anypath" "autolab" "checkvar" "ck1icd9" "ckicd9"
	;; 	 "dtapath" "dupclean" "echo"
	;; 	 "getdate" "getlbl" "getnames" "getobs"
	;; 	 "icd9x" "issorted" "isfile" "jultoe" "jultof" "jultomdy" "knowndup"
	;; 	 "labeldir" "linker" 
	;; 	 "markit" "makewide" "missize" "mpcounts" 
	;; 	 "nodups" "notefile" "prov2zip"
	;; 	 "qcolsum" "qorder" 
	;; 	 "random" "readraw" "readzip" "repart"
	;; 	 "setup" "stdrate"
	;; 	 "timeslot"
	;; 	 "_addext" "_brclean" "_brckado" "_brdlog"
	;; 	 "_ckbad" "_ckdunno" "_ckdupl" "_ckmiss" "_ckok" "_ckwarn"
	;; 	 "_delimit" "_filenm" "_lookup" "_mk_ck"
	;; 	 ) 'words))
	;;   '(1 ado-builtin-harmless-face))

	;;
	;; now, presenting smcl
	;;
	;; Syntax 1
	(list
	  (concat
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "asis"
		  "bf" "break"
		  "cmd" "com"
		  "depvar" "depvars" "depvarlist" "dtype"
		  "err" "error"
		  "hi" "hilite" "hline"
		  "ifin" "imp" "indepvars" "input" "it"
		  "newvar"
		  "p" "p_end" "p2colreset" "p2line" 
		  "phang" "phang2" "phang3" "pin" "pin2" "pin3" "pmore" "pmore2" "pmore3" "psee" "pstd"
		  "res" "reset" "result"
		  "s6hlp"
		  "sf" "smcl" "synopthdr" "synoptline" "synoptset"
		  "tab" "text" "txt"
		  "var" "varlist" "varname" "vars"
		  "weight"
		 ) 'words))
	   "[ \t]*?"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t) '(3 ado-constant-face))
	;; Syntax 1 with funny chars (need to build regexp by hand)
	(list
	  (concat
	   "\\({\\)"
	   "[ \t]*"
	   "\\([.]\\(?:[.][.]\\|-\\)\\)"
	   "[ \t]*?"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t) '(3 ado-constant-face))

	  ;; Syntax 2
	(list
	  (concat
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "ado_d"
		  "back" "bf" "bind" 
		  "center" "centre" "clearmore" "cmd"
		  "depvar" "depvars" "depvarlist" "dlgtab"
		  "err" "error"
		  "help_d" "hi" "hilite"
		  "indepvars" "inp" "input" "it"
		  "net_d" "netfrom_d" "news" "newvar"
		  "p2col" "p2coldent"
		  "rcenter" "rcentre" "res" "reset" "result" "right"
		  "search_d"
		  "sf" "synopt" "synopthdr" "syntab"
		  "text" "title" "txt"
		  "ul" "update_d"
		  "var" "varlist" "varname" "vars" "view_d"
		 ) 'words))
	   "[ \t]*"
	   "\\(:\\)"
	   "\\([^}]+\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t)
	  '(3 ado-constant-face) '(4 ado-subcommand-face t) '(5 ado-constant-face))
	  ;; making comments look like comments
	(list
	 (concat
	  "\\({\\)"
	  "[ \t]*"
	  "\\([*]\\)"
	  "[ \t]*?"
	  "\\(:\\)"
	  "\\([^}]+\\)"
	  "\\(}\\)"
	  )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t)
	  '(3 ado-constant-face) '(4 ado-comment-face t) '(5 ado-constant-face))

	  ;; Syntax 2plus for cmdab
	(list
	  (concat
	   "\\({\\)"
	   "[ \t]*"
	   "\\<\\(cmdab\\)\\>"
	   "\\(:\\)"
	   "\\([a-zA-Z][a-zA_Z_0-9]*\\)"
	   "\\(:\\)"
	   "\\([^}]+\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face)
	  '(3 ado-constant-face) '(4 ado-subcommand-face) '(5 ado-constant-face)
	  '(6 ado-subcommand-face) '(7 ado-constant-face))

	  ;; Syntax 3 with free form args
	(list
	  (concat
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "ado"
		  "browse"
		  "c" "char"
		  "dialog"
		  "help" "helpb"
		  "manlink" "manpage" "mansection" "marker" "matacmd"
		  "net"
		  "opt"
		  "search" "stata"
		  "update"
		  "view"
		 ) 'words))
	   "[ \t]+"
	   "\\([^:]*?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face))
	  ;; Syntax 3 with on/off choices
	(list
	  (concat
	   "\\({\\)"
	   "[ \t]*"
	   "\\<\\(ul\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "off" "on"
		 ) 'words))
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face))
	   
	  ;; Syntax 3 comments
	(list
	  (concat
	   "\\({\\)"
	   "[ \t]*"
	   "\\([*]\\)"
	   "\\(.*?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t)
	  '(3 ado-comment-face t) '(4 ado-constant-face))
	  ;; Syntax 3 with single numerical args --- allow simple macros, too
	(list
	  (concat
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "col" "hline" "space" "synoptset"
		 ) 'words))
	   "[ \t]+"
	   "\\(\\(?:[0-9]+\\|`[a-zA-Z0-9_`']*'\\)\\)"
	   "[ \t]*"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face))
	  ;; Syntax 3 with a single numerical arg and a choice (nice syntax)
	(list
	  (concat
	   "\\({\\)"
	   "[ \t]*"
	   "\\<\\(synoptset\\)\\>"
	   "\\(\\(?:[ \t]+\\(?:[0-9]+\\|`[a-zA-Z0-9_`']*'\\)\\)?\\)"
	   "\\(\\(?:[ \t]+\\(?:notes\\|tabbed\\)\\)?\\)"
	   "[ \t]*"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t)
	  '(3 ado-subcommand-face t) '(4 ado-subcommand-face t)
	  '(5 ado-constant-face))
	  ;; Syntax 3 with 0 through 4 numerical args --- allow simple macros, too
	(list
	  (concat
	   "\\({\\)"
	   "[ \t]*"
	   "\\<\\(p\\)\\>"
	   "\\(\\(?:[ \t]+\\(?:[0-9]+\\|`[a-zA-Z0-9_`']*'\\)\\)\\{0,4\\}\\)"
	   "[ \t]*"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face))
	  ;; Syntax 3 with exactly 4 numerical args --- allow simple macros, too
	(list
	  (concat
	   "\\({\\)"
	   "[ \t]*"
	   "\\<\\(p2colset\\)\\>"
	   "\\(\\(?:[ \t]+\\(?:[0-9]+\\|`[a-zA-Z0-9_`']*'\\)\\)\\{4\\}\\)"
	   "[ \t]*"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face))
	  ;; Syntax 3 with exactly 2 numerical arguments. 
	;; Why doesn't SMCL come close to having a clean syntax?
	(list
	  (concat
	   "\\({\\)"
	   "[ \t]*"
	   "\\<\\(p2line\\)\\>"
	   "\\(\\(?:[ \t]+\\(?:[0-9]+\\|`[a-zA-Z0-9_`']*'\\)\\)\\{2\\}\\)"
	   "[ \t]*"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face t)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face))
       
	  ;; Syntax 4
	  ;; those whose subcommands are not easy
	  ;;  on second thought: for all!
	(list
	  (concat
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "ado"
		  "browse"
		  "dialog"
		  "help" "helpb"
		  "manpage" "mansection" "matacmd"
		  "net"
		  "opt"
		  "search" "stata"
		  "update"
		  "view"
		 ) 'words))
	   "[ \t]+"
	   "\\(.*?\\)"
	   "\\(:\\)"
	   "\\([^}]+?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face prepend)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face t)  
	  '(5 ado-subcommand-face t) '(6 ado-constant-face))

	  ;; syntax 4 with a numeric first argument (for dup)
	(list
	  (concat
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
			"center" "centre" 
			"dlgtab"
			"dup"
			"lalign"
			"ralign"
			"rcenter" "rcentre"
		 ) 'words))
	   "[ \t]+"
	   "\\([1-9][0-9]*\\)"
	   "[ \t]*"
	   "\\(:\\)"
	   "\\([^}]+?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face prepend)
	  '(3 ado-subcommand-face) '(4 ado-constant-face t)  
	  '(5 ado-subcommand-face t) '(6 ado-constant-face))
	(list
	  (concat
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
			"center" "centre" 
			"dlgtab"
			"dup"
			"lalign"
			"ralign"
			"rcenter" "rcentre"
		 ) 'words))
	   "[ \t]+"
	   "\\(`[a-zA-Z0-9_`']*'\\)"
	   "[ \t]*"
	   "\\(:\\)"
	   "\\([^}]+?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face prepend)
	  '(3 ado-variable-name-face) '(4 ado-constant-face t)  
	  '(5 ado-subcommand-face t) '(6 ado-constant-face))
	  ;; Syntax 4 with exactly 2 numerical args to start with
	(list
	  (concat
	   "\\({\\)"
	   "[ \t]*"
	   "\\<\\(dlgtab\\)\\>"
	   "\\(\\(?:[ \t]+\\(?:[0-9]+\\|`[a-zA-Z0-9_`']*'\\)\\)\\{2\\}\\)"
	   "[ \t]*"
	   "\\(:\\)"
	   "\\([^}]+?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face prepend)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face t)  
	  '(5 ado-subcommand-face t) '(6 ado-constant-face))
	  ;; Syntax 4 with exactly 4 numerical args to start with
	(list
	  (concat
	   "\\({\\)"
	   "[ \t]*"
	   "\\<\\(p2col\\)\\>"
	   "\\(\\(?:[ \t]+\\(?:[0-9]+\\|`[a-zA-Z0-9_`']*'\\)\\)\\{4\\}\\)"
	   "[ \t]*"
	   "\\(:\\)"
	   "\\([^}]+?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face prepend)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face t)  
	  '(5 ado-subcommand-face t) '(6 ado-constant-face))

	  ;; Syntax 3
	  ;; for the manhelp and manhelpi exceptions
	(list
	  (concat
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "manhelp" "manhelpi"
		 ) 'words))
	   "[ \t]+"
	   "\\(\\w*?\\)"
	   "[ \t]+"
	   "\\(\\w*?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face prepend)
	  '(3 ado-subcommand-face t) '(4 ado-subcommand-face t) 
	  '(5 ado-constant-face))

	  ;; Syntax 4 exceptions
	(list
	  (concat
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "manhelp" "manhelpi"
		 ) 'words))
	   "[ \t]+"
	   "\\(\\w*?\\)"
	   "[ \t]+"
	   "\\(\\w*?\\)"
	   "\\(:\\)"
	   "\\([^}]+?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face prepend)
	  '(3 ado-subcommand-face t) '(4 ado-subcommand-face t) 
	  '(5 ado-constant-face t) '(6 ado-subcommand-face t) 
	  '(7 ado-constant-face t))

	  ;; special help syntax with double-hash
	  ;;  sheesh --- should be able to reuse with optional stuff
	(list
	  (concat
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "help" "helpb"
		 ) 'words))
	   "[ \t]+"
	   "\\([^:# \t]*\\)"
	   "\\(##\\)"
	   "\\(\\w*?\\)"
	   "\\(\\(?:|\\w*?\\)?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face t) 
	  '(5 ado-subcommand-face t) '(6 ado-subcommand-face t) 
	  '(7 ado-constant-face t))
	(list
	  (concat
	   "\\({\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "help" "helpb"
		 ) 'words))
	   "[ \t]+"
	   "\\([^:# \t]*\\)"
	   "\\(##\\)"
	   "\\(\\w*?\\)"
	   "\\(\\(?:|\\w*?\\)?\\)"
	   "[ \t]*"
	   "\\(:\\)"
	   "\\([^}]*?\\)"
	   "\\(}\\)"
	   )
	  '(1 ado-constant-face) '(2 ado-builtin-harmless-face)
	  '(3 ado-subcommand-face t) '(4 ado-constant-face t) 
	  '(5 ado-subcommand-face t) '(6 ado-subcommand-face t) 
	  '(7 ado-constant-face t) '(8 ado-subcommand-face t) 
	  '(9 ado-constant-face t))
    
	  ;; class stuff
;;; builtin prefix operators
	(list
	  (concat
	   "\\([.]\\)"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "Global" "Local" "Super"
		 ) 'words))
	   "\\(?:[.a-zA-Z][a-zA-Z0-9_]*\\)+"
	   )
	  '(1 ado-function-name-face) '(2 ado-function-name-face t))

;;; builtin class functions and modifiers
	(list
	 (concat
;;	  "\\(?:[.a-zA-Z][a-zA-Z0-9_]*\\)+" ! will cause a hang
	  "\\([.]\\)"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "Arrdropall" "Arrdropel" "Arrpop" "Arrpush"
		   "Declare"
		   "arrindexof" "arrnels"
		   "classmv" "classname" "copy"
		   "dynamicmv"
		   "instancemv" "isa" "isofclass"
		   "new"
		   "objkey" "objtype"
		   "ref" "ref_n"
		   "superclass"
		  "uname" 
		  ) 'words))
	  )
	 '(1 ado-function-name-face) '(2 ado-function-name-face t))

;;; class command
	(list
	 (concat
	  "\\<\\(class\\)\\>"
	  "[ \t]+"
	  "\\<\\(exit\\)\\>"
	  )
	 '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))
	(list
	  (concat
	  "\\<\\(class\\)\\>"
	  "[ \t]+"
	  "\\([_a-zA-Z][_a-zA-Z0-9]*\\)"
	  )
	  '(1 ado-builtin-harmful-face) '(2 ado-builtin-harmful-face t))

     ;; all the different declarations
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "class" "classw" "classwi" "classwid" "classwide" 
		  "instance" "instances" "instancesp" "instancespe" "instancespec" 
		  "instancespeci" "instancespecif" "instancespecifi" "instancespecific" 
		 ) 'words))
	   "[ \t]*:"
	   )
	  '(1 ado-builtin-harmless-face))

	  ;; classutil stuff
	(list
	  (concat
	   "\\<\\(classutil\\)\\>"
	   "[ \t]+"
	   "\\<\\(drop\\)\\>"
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-subcommand-face t))

	(list
	  (concat
	   "\\<\\(classutil\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "cdir"
		  "d" "de" "des" "desc" "descr" "descri" "describ" "describe"
		  "dir"
		  "which"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	  ;; oh my - the creturn info!
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "cret" "cretu" "cretur" "creturn"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"l" "li" "lis" "list" 
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

;;; the system 'constants' (which are not really constant) - the c() thingies
	(list
	  (concat
	   "\\(c(\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "ALPHA" "Mons" "Months" "MP" "N" "SE" "Wdays" "Weekdays"
		  "adopath" "adosize" "alpha" 
		  "born_date" "byteorder"
		  "changed" "checksum" "cmdlen" "console" "copycolor" "current_time" "current_date"
		  "dirsep" "dp"
		  "eolchar" "epsdouble" "epsfloat" "eqlen"
		  "filedate" "filename" "flavor"
		  "graphics" 
		  "httpproxy" "httpproxyauth" "httpproxyhost" "httpproxyport" "httpproxypw" "httpproxyuser"
		  "icmap"
		  "k"
		  "level" "linegap" "linesize" "logtype"
		  "machine_type" "macrolen" 
		  "matacache" "matafavor" "matalibs" "matalnum" "matamofirst" "mataoptimize" "matastrict"
		  "matsize" 
		  "max_N_current" "max_N_theory" "max_cmdlen" 
		  "max_k_current" "max_k_theory" 
		  "max_macrolen" "max_matsize" "max_width_current" "max_width_theory" 
		  "maxbyte" "maxdb" "maxdouble" "maxfloat" "maxint" "maxiter" "maxlong" "maxstrvarlen" "maxvar" 
		  "memory"
		  "min_matsize"
		  "minbyte" "mindouble" "minfloat" "minint" "minlong"
		  "mode" "more"
		  "namelen" "noisily"
		  "odbcmgr" "os" "osdtl"
		  "pagesize" "pi" "printcolor" "processors" "processors_lic" "processors_mach" "processors_max" "pwd"
		  "rc" "reventries" "rmsg" "rmsg_time"
		  "scheme" "scrollbufsize" "searchdefault" "seed" 
		  "smallestdouble" "stata_version"
		  "sysdir_base" "sysdir_oldplace" "sysdir_personal" "sysdir_plus" "sysdir_site" "sysdir_stata"
		  "sysdir_updates" 
		  "timeout1" "timeout2" "tmpdir"
		  "trace" "tracedepth" "traceexpand" "tracehilite" "traceindent" "tracenumber" "tracesep" "type"
		  "username"
		  "varabbrev" "version" "virtual"
		  "width" 
			) 'words))
	   "[ \t]*"
	   "\\()\\)"
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-constant-face t)
	  '(3 ado-builtin-harmless-face))

;;; platform specific c() thingies
	(list
	  (concat
	   "\\(c(\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"autotabgraphs"
			"dockable" "dockingguides" "doublebuffer"
			"eolchar"
			"locksplitters"
			"notifyuser"
			"playsnd"
			"persistfv" "persistvtopic" "pinnable"
			"revkeyboard"
			"smoothfonts"
			"update_interval" "update_prompt" "update_query"
			"varkeyboard" "varlabelpos"
			) 'words))
	   "[ \t]*"
	   "\\()\\)"
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-platform-specific-face t)
	  '(3 ado-builtin-harmless-face))
;;; obsolete c() thingies
	(list
	  (concat
	   "\\(c(\\)"
	   "[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"fastscroll" "floatresults" "floatwindows"
			"macgphengine"
			"smalldlg"
			"piccomments"
			"revwindow"
			"smalldlg" 
			"smoothsize"
			"use_atsui_graph" "use_qd_text"
			"varwindow"
			"xptheme"
			) 'words))
	   "[ \t]*"
	   "\\()\\)"
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-obsolete-face t)
	  '(3 ado-builtin-harmless-face))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stuff for writing dlg files  ;;;
;;; should be its own minor mode ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "VERSION"
		 ) 'words))
	   "[ \t]+"
	   "\\(\\(?:\\(?:9[.]00\\|\\(?:8\\|9\\|\\(?:1\\(?:0\\|1\\)\\)\\)[.]\\(?:0\\|1\\)\\)\\|\\(?:\\(?:8\\|9\\)[.]2\\)\\|\\(?:[89]\\|1\\(?:0\\|1\\)\\)\\)\\)"
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))
	  ;; general builtins for dialogs
	  ;; here - the harmless faces define static text 
	  ;;        whereas the harmful face defines dynamic text
	(list
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "BUTTON"
		  "CANCEL" "CHECKBOX" "COMBOBOX"
		  "DEFINE" "DIALOG"
		  "EDIT" 
		  "FILE"
		  "HELP"
		  "INCLUDE"
		  "LISTBOX"
		  "OK"
		  "RADIO" "RESET"
		  "SPINNER" "SUBMIT"
		  "VARLIST" "VARNAME"
		 ) 'words))
	  '(1 ado-builtin-harmful-face))
	(list
	  (concat
	   "\\<\\(stopbox\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"note"
			"rusure"
			"stop"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

	(list
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "BEGIN"
		  "COLOR"
		  "END" "EXP"
		  "FRAME"
		  "GROUPBOX"
		  "LIST"
		  "POSITION" "PROGRAM"
		  "SCRIPT"
		  "TEXT"
		  "allowxi"
		  "beginoptions" "by" "bysort"
		  "endoptions" "exit"
		  "ifexp" "inrange"
		  "option" "optionarg"
		  "put"
		  "require"
		  "stata"
		  "varlist"
		  "weight"
		 ) 'words))
	  '(1 ado-builtin-harmless-face))

	(list
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "action"
		  "gaction"
		  "script"
		  "view"
		  "program"
		 ) 'words))
	  '(1 ado-function-name-face))
	(list
	  (concat
	   "\\<\\(call\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "action"
		  "gaction"
		  "script"
		  "view"
		  "program"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

	  ;; stata dialog functions i.e. things which require () after them 
	(list
	  (concat
	   "\\>"
	   "\\([.]\\)"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"contains"
			"endswith"
			"isdefault" "isenabled" "iseq" "iseqignorecase" "isge" "isgt" "isle" "islt" "isneq" "isvisible"
			) t))
	   "("
	   )
	  '(1 ado-function-name-face) '(2 ado-function-name-face t))

	  ;; mata keywords --- won't override, because they are only active in a mata block...	  ;;  and mata block checking has not been implemented
	(list
	 (concat
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		   "break"
		   "colvector" "complex" "continue"
		   "do"
		   "external"
		   "for" "function"
		   "goto"
		   "if" 
		   "matrix"
		   "numeric"
		   "pointer" "pragma" 
		   "real" "return" "rowvector"
		   "scalar" "string" 
		   "transmorphic" 
		   "vector" "version" "void"
		   "while"
		   ) 'words))
	  )
	 '(1 ado-mata-keyword-face))

	(list
	 (concat
	  "[ \t]+"
	  (eval-when-compile 
		(regexp-opt 
		 '(
		  "aggregate" "array"
		  "boolean" "byte" 
		  "case" "catch" "class" "const"
		  "default" "delegate" "delete" "double" 
		  "else" "eltypedef" "end" "enum" "explicit" "export"
		  "float" "friend" 
		  "global"
		  "inline" "int" 
		  "local" "long" 
		  "namespace" "new" "NULL" 
		  "operator" "orgtypedef"
		  "polymorphic" "private" "protected" "public"
		  "quad"
		  "short" "signed" "static" "struct" "super" "switch"
		  "template" "this" "throw" "try" "typedef" "typename"
		  "union" "unsigned" "using" 
		  "virtual" "volatile"
		   ) 'words))
	  )
	 '(1 ado-mata-future-keyword-face))

	(list
	  (concat
	   "\\<\\(pragma\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"unset" "unused"
			) 'words))
	   )
	  '(1 ado-mata-keyword-face) '(2 ado-subcommand-face))
    
	(list
	  (concat
	   "\\<"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"pointer" "return"
			) t))
	   "("
	   )
	  '(1 ado-mata-keyword-face))

	  ;; mata subcommands
	  ;;  does this run into the need for extra harmful and harmless faces?!?!
	(list
	  (concat
	   "\\<\\(mata\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "clear"
		  "d" "de" "des" "desc" "descr" "descri" "describ" "describe" 
		  "drop"
		  "end"
		  "help"
		  "matd" "matde" "matdes" "matdesc" "matdescr" "matdescri" "matdescrib" "matdescribe" 
		  "matsave" "matuse" "memory" "mlib" "mosave"
		  "query"
		  "rename"
		  "set" "stata"
		  "which"
			) 'words))
	   )
	  '(1 ado-mata-keyword-face t) '(2 ado-mata-keyword-face t))

	  ;; mata subcommands
	  ;;  does this run into the need for extra harmful and harmless faces?!?!
	(list
	  (concat
	   "\\<\\(mata\\)\\>"
	   "[ \t]+"
	   "\\<\\(mlib\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "add"
		  "create"
		  "index"
		  "q" "qu" "que" "quer" "query"
			) 'words))
	   )
	  '(1 ado-mata-keyword-face t) '(2 ado-mata-keyword-face t) 
	  '(3 ado-subcommand-face t))

	  ;; general mata set commands
	(list
	  (concat
	   "\\<\\(mata\\)\\>"
	   "[ \t]+"
	   "\\<\\(set\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"matacache" "matalibs"
			) 'words))
	   )
	  '(1 ado-mata-keyword-face t) '(2 ado-mata-keyword-face t) 
	  '(3 ado-subcommand-face t))

	  ;; general mata set on/off commands
	(list
	  (concat
	   "\\<\\(mata\\)\\>"
	   "[ \t]+"
	   "\\<\\(set\\)\\>"
	   "[ \t]+"
	   "\\<\\(matafavor\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"space" "speed"
			) 'words))	   
	   )
	  '(1 ado-mata-keyword-face t) '(2 ado-mata-keyword-face t) 
	  '(3 ado-subcommand-face t) '(4 ado-subcommand-face t))

	  ;; mata functions (durn, this is a pain in the butt)
	  ;; functions which exist for regular stata are NOT included
	  ;; these, too ended up being split

	  ;; _ functions... perhaps the _ should be split off?
	(list
	  (concat
	   (eval-when-compile 
		 "\\b"
		 (regexp-opt 
		  '(
		  "_chdir" "_cholesky" "_cholinv" "_cholsolve" "_collate" "_conj" "_corr"
		  "_deriv" "_deriv_result_Hessian" "_deriv_result_Jacobian" "_deriv_result_gradient" "_deriv_result_scores" "_diag"
		  "_editmissing" "_edittoint" "_edittointtol" "_edittozero" "_edittozerotol" "_editvalue"
		  "_eigensystem" "_eigenvalues"
		  "_equilc" "_equilr" "_equilrc" "_error" 
		  "_fft" "_fillmissing" 
		  "_fclose" "_fget" "_fgetmatrix" "_fgetnl" "_flopin" "_flopout" "_fopen" "_fput" "_fputmatrix" "_fread" 
		  "_fseek" "_ftell" "_ftruncate" "_fullsvd" "_fwrite"
		  "_ghessenbergd"
		  "_gschurd" "_gschurdgroupby"
		  "_halton" "_hessenbergd" "_hqrd" "_hqrdp" "_hqrdp_la"
		  "_invfft" "_invsym"
		  "_jumble"
		  "_lefteigensystem" "_lowertriangle" "_lud" "_lud_la" "_luinv" "_luinv_la" "_lusolve" "_lusolve_la"
		  "_makesymmetric" "_matexpsym" "_matlogsym" "_matpowersym" "_mkdir"
		  "_moptimize" "_moptimize_evaluate"
		  "_negate"
		  "_optimize" "_optimize_evaluate"
		  "_perhapsequilc" "_perhapsequilr" "_perhapsequilrc" "_pinv"
		  "_qrinv" "_qrsolve" "_quadrunningsum"
		  "_rmdir" "_runningsum"
		  "_schurd" "_solvelower" "_solveupper" "_sort" 
		  "_st_addobs" "_st_addvar" 
		  "_st_data" "_st_macroexpand" 
		  "_st_sdata" "_st_sstore" "_st_store"
		  "_st_tsrevar"
		  "_st_varindex"
		  "_stata" "_strtoreal" "_sublowertriangle" "_substr" "_svd" "_svd_la" "_svdsv" "_svsolve" "_symeigensystem" "_symeigenvalues"
		  "_transpose" "_transposeonly"
		  "_unlink" "_uppertriangle" 
		 ) t))
	   "("
	   )
	  '(1 ado-mata-function-name-face t))
	;; mata functions
	(list
	  (concat
	   "\\<\\(mata\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "clear"
		  "d" "de" "des" "desc" "descr" "descri" "describ" "describe" 
		  "drop"
		  "end"
		  "help"
		  "matd" "matde" "matdes" "matdesc" "matdescr" "matdescri" "matdescrib" "matdescribe" 
		  "matsave" "matuse" "memory" "mlib" "mosave"
		  "query"
		  "rename"
		  "set" "stata"
		  "which"
			) 'words))
	   )
	  '(1 ado-mata-keyword-face t) '(2 ado-mata-keyword-face t))
	(list
	 (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "C" "Corr" "Dmatrix" "Hilbert" "Im" "Kmatrix" 
		  "LA_DGEBAK" "LA_DGEBAL" "LA_DGEES" "LA_DGEEV" "LA_DGEHRD" 
		  "LA_DGGBAK" "LA_DGGBAL" "LA_DGGHRD" 
		  "LA_DHGEQZ" 
		  "LA_DHSEIN" "LA_DHSEQR" 
		  "LA_DLAMCH" "LA_DORGHR" "LA_DSYEVX" 
		  "LA_DTGSEN" "LA_DTGEVC" "LA_DTREVC" "LA_DTRSEN" 
		  "LA_ZGEBAK" "LA_ZGEBAL" "LA_ZGEES" "LA_ZGEEV" "LA_ZGEHRD" 
		  "LA_ZGGBAK" "LA_ZGGBAL" "LA_ZGGHRD" 
		  "LA_ZHGEQZ" "LA_ZHSEIN" "LA_ZHSEQR" 
		  "LA_ZTGSEN" "LA_ZTGEVC" "LA_ZTREVC" "LA_ZTRSEN" "LA_ZUNGHR"
		  "Lmatrix"
		  "Re" "Toeplitz" "Vandermonde"
		  "acosh" "acosr" "adosubdir" "all" "allof" "any" "anyof" 
		  "arg" "args" "asinh" "asinr"
		  "asarray" "asarray_contains" "asarray_contents" "asarray_create" "asarray_elements"
		  "asarray_first" "asarray_key" "asarray_keys" "asarray_next" 
		  "asarray_notfound" "asarray_remove"
		  "ascii" "assert" "asserteq" "atanh" "atanr"
		  "blockdiag" "breakkey" "breakkeyreset" 
		  "bufbfmtisnum" "bufbfmtlen" "bufbyteorder" "bufget" "bufio" "bufmissingvalue" "bufput"
		  "callersversion" "cat" "chdir" "cholsolve" "cholinv" 
		  "colmax" "colmaxabs" "colmin" "colminmax" "colmissing" "colnonmissing" "cols" "colscalefactors" "colshape" "colsum" 
		  "conj" "convolve" "correlation" "cosh" "crexternal" "cross" "crossdev" "cvpermute" "cvpermutesetup"
		  "deconvolve"
		  "deriv"
		  "deriv_init_argument"
		  "deriv_init"
		  "deriv_init_bounds"
		  "deriv_init_evaluator" "deriv_init_evaluatortype"
		  "deriv_init_h"
		  "deriv_init_narguments"
		  "deriv_init_params"
		  "deriv_init_scale" "deriv_init_search"
		  "deriv_init_verbose"
		  "deriv_init_weights"
		  "deriv_query"
		  "deriv_result_Hessian" "deriv_result_Jacobian"
		  "deriv_result_delta"
		  "deriv_result_errorcode" "deriv_result_errortext"
		  "deriv_result_gradient"
		  "deriv_result_h"
		  "deriv_result_returncode"
		  "deriv_result_scale" "deriv_result_scores"
		  "deriv_result_value" "deriv_result_values"
		  "designmatrix" "dettriangular"
		  "diag" "diagonal" "dir" "direxists" "direxternal" "display" "displayas" "displayflush" "dsign"
		  "editmissing" "edittoint" "edittointtol" "edittozero" "edittozerotol" "editvalue" 
		  "eigensystem" "eigensystemselectf" "eigensystemselecti" "eigensystemselectr" "eigenvalues" 
		  "eltype" "epsilon" "error" "errprintf" "exit"
		  "factorial" "favorspeed" "fbufget" "fbufput" "fclose" 
		  "ferrortext" "fft" "fget" "fgetnl" "fgetmatrix" 
		  "fileexists" "findexternal" "findfile" "floatround" "fopen" "fput" "fputmatrix" 
		  "fread" "freturncode" "frombase" "fseek" "fstatus" 
		  "ftell" "ftfreqs" "ftpad" "ftperiodogram" "ftretime" "ftruncate" "ftunwrap" "ftwrap" 
		  "fullsdiag" "fullsvd" "fwrite"
		  "gamma"
		  "geigensystem" "geigensystemelectf" "geigensystemelecti" "geigensystemelectr"
		  "ghalton" "ghessenbergd" 
		  "ghk" "ghk_init" "ghk_init_antithetics" "ghk_init_method" "ghk_init_pivot" "ghk_init_start" 
		  "ghk_query_npts"
		  "ghkfast"
		  "ghkfast_i" 
		  "ghkfast_init" "ghkfast_init_antithetics" "ghkfast_init_pivot"
		  "ghkfast_query_dim"
		  "ghkfast_query_method"
		  "ghkfast_query_n"
		  "ghkfast_query_npts"
		  "ghkfast_query_pointset_i"
		  "ghkfast_query_rseed"
		  "gschurd" "gschurdgroupby"
		  "halton" "hash1" "hasmissing" "hessenbergd" "hqrd" "hqrdp" "hqrdmultq" "hqrdmultqlt" "hqrdq" "hqrdq1" "hqrdr" "hqrdr1"
		  "inbase" "invHilbert" "invfft" "invorder" "invtokens" "invvech"
		  "iscomplex" "isdiagonal" "isfleeting" "ispointer" "isreal" 
		  "isrealvalues" "isstring" "issymmetric" "issymmetriconly" "isview" 
		  "jumble"
		 ) 'words))
	   "("
	   )
	  '(1 ado-mata-function-name-face t))
	(list
	 (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "lefteigensystem" "lefteigensystemselectf" "lefteigensystemselecti" "lefteigensystemselectr"
		  "leftgeigensystem" "leftgeigensystemelectf" "leftgeigensystemelecti" "leftgeigensystemelectr"
		  "liststruct" "lnnormal" "lnnormalden" "lowertriangle" "lud" "luinv" "lusolve"
		  "makesymmetric" "matexpsym" "matlogsym" "matpowersym" "maxindex"
		  "mean" "meanvariance" "minindex" "minmax" "missingof" 
		  "moptimize" "moptimize_ado_cleanup" "moptimize_evaluate" "moptimize_init" "moptimize_query"
		  "more"
		  "mreldifre" "mreldifsym"
		  "nameexternal" "nonmissing" "norm"
		  "optimize" "optimize_evaluate" "optimize_init" "optimize_query"
		  "order" "orgtype"
		  "panelsetup" "panelstats" "panelsubmatrix" "panelsubview"
		  "pathasciisuffix" "pathbasename" "pathisabs" "pathisurl" "pathjoin" "pathlist" 
		  "pathrmsuffix" "pathsearchlist" "pathsplit" "pathstatasuffix" "pathsubsysdir" "pathsuffix"
		  "pi" "pinv"
		  "polyadd" "polyderiv" "polydiv" "polyeval" "polyinteg" "polymult" "polyroots" "polysolve" "polytrim"
		  "printf" "pwd"
		  "range" "rangen" "rmexternal" "rowmax" "rowmissing" "rowscalefactors"
		  "qrd" "qrdp" "qrinv" "qrsolve" 
		  "quadcorrelation" "quadcross" "quadcrossdev" "quadrant" "quadcolsum" 
		  "quadmeanvariance" "quadrowsum" "quadrunningsum" "quadsum" "quadvariance" 
		  "querybreakintr"
		  "rank" "rdiscrete" "revorder" "rowmaxabs" "rowmin" "rowminmax" "rownonmissing" "rows" "rowshape" "rowsum" "rseed" "runningsum"
		  "schurd" "select"
		  "setbreakintr" "setmore" "setmoreonexit" "sinh" "sizeof" "smallestdouble"
		  "solve_tol" "solvelower" "solveupper"
		  "sort" "spline3" "spline3eval" "sprintf"
		  "st_select" "stata" "statasetversion" "stataversion" 
		  "stritrim" "strltrim" "strreverse" "strrtrim" "strtoreal" "strtrim" "strlower" "strproper" "strupper"
		  "sublowertriangle"
		  "svd" "svdsv" "svsolve" "swap"
		  "symeigensystem" "symeigensystemselecti" "symeigensystemselectr" "symeigenvalues"
		  "timer" 
		  "tokenallowhex" "tokenallownum" "tokenget" "tokengetall" "tokeninit" "tokeninitstata" "tokenoffset" 
		  "tokenpeek" "tokenrest" "tokens" "tokenset" "tokenpchars" "tokenqchars" "tokenwchars" "transposeonly"
		  "uniqrows" "unitcircle"
		  "valofexternal" "variance" "vec" "vech"
		  "unlink" "unorder" "uppertriangle"
		 ) 'words))
	   "("
	   )
	 '(1 ado-mata-function-name-face t))


	  ;; the moptimize_init functions
	(list
	  (concat
	   "\\<\\(moptimize_init_\\)"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			 "by" 
			 "cluster" "constraints" 
			 "conv_ignorenrtol" "conv_maxiter" "conv_nrtol" "conv_ptol" "conv_vtol" "conv_warning" 
			 "depvar" 
			 "eq_coefs" "eq_colnames" "eq_cons" "eq_exposure" "eq_indepbars" "eq_n" "eq_name" "eq_offset" 
			 "evaluations" "evaluator" "evaluatortype" 
			 "gnweightmatrix" 
			 "iterid" 
			 "ndepvars" "negH" "nmsimplexdeltas" "nuserinfo" 
			 "search" "search_bounds" "search_random" "search_repeat" "search_rescale" 
			 "singularHmethod" "svy" 
			 "technique" "touse" 
			 "trace_Hessian" "trace_ado" "trace_coefs" "trace_dots" "trace_gradient" "trace_step" "trace_tol" "trace_value" 
			 "tracelevel" 
			 "userinfo" 
			 "valueid" "vcetype" "verbose" "view" 
			 "weight" "weighttype" "which" 			 
			) t ))
	   "("
	   )
	  '(1 ado-mata-function-name-face t) '(2 ado-mata-function-name-face t))

	  ;; the moptimize results prefix functions
	(list
	  (concat
	   "\\<\\(moptimize_result_\\)"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"Hessian" 
			"V" "V_oim" "V_opg" "V_robust" "Vtype" 
			"coefs" "colstripe" "converged" 
			"display" 
			"errorcode" "errortext" "evaluations" 
			"gradient" 
			"interations" "iterationlog" 
			"post" 
			"returncode" 
			"scores" 
			"value" "value0" 			 
			) t ))
	   "("
	   )
	  '(1 ado-mata-function-name-face t) '(2 ado-mata-function-name-face t))

	  ;; the moptimize_util prefix functions
	(list
	  (concat
	   "\\<\\(moptimize_util_\\)"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"depvar" "eq_indices" "matbysum" "matsum" "sum" "vecsum" "xb" 						) t ))
	   "("
	   )
	  '(1 ado-mata-function-name-face t) '(2 ado-mata-function-name-face t))
	  
	  ;; arrrgh the mata st_ functions
	(list
	  (concat
	   "\\<\\(st_\\)"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "addobs" "addvar" 
		  "data" "dir" "dropobsif" "dropobsin" "dropvar"
		  "eclear"
		  "global"
		  "isfmt" "islmname" "isname" "isnumfmt" "isnumvar" "isstrfmt" "isstrvar"
		  "keepobsif" "keepobsin" "keepvar"
		  "local"
		  "macroexpand" "matrix" "matrixcolstripe" "matrixrowstripe"
		  "nobs" "numscalar" "nvar"
		  "rclear" "replacematrix"
		  "sclear" "sdata" "sstore" "store" "strscalar" "subview" "sview"
		  "tempfilename" "tempname" "tsrevar" 
		  "updata"
		  "varformat" "varindex" "varlabel" "varname" "varrename" "vartype" "varvaluelabel"
		  "view" "viewobs" "viewvars"
		  "vldrop" "vlexists" "vlload" "vlmap" "vlmodify" "vlsearch"
			) t ))
	   "("
	   )
	  '(1 ado-mata-function-name-face t) '(2 ado-mata-function-name-face t))

	  ;; mata optimize_init commands
	(list
	  (concat
	   "\\<\\(optimize_init_\\)"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"argument"
			"cluster" "colstripe" "constraints" "conv_maxiter" "conv_ptol" "conv_warning"
			"evaluations" "evaluator" "evaluatortype"
			"gnweightmatrix" 
			"ingnorenrtol" "iterid"
			"narguments" "negH" "nmsimplexdeltas" "nrtol"
			"params"
			"singularHmethod"
			"technique" 
			"trace_Hessian" "trace_dots" "trace_gradient" "trace_params" "trace_step" "trace_tol" "trace_value"
			"tracelevel" "type"
			"valueid" "verbose" "vtol"
			"which"
			) t ))
	   "("
	   )
	  '(1 ado-mata-function-name-face t) '(2 ado-mata-function-name-face t))

	(list
	  (concat
	   "\\<\\(optimize_result_\\)"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			  "Hessian"
			  "V" "V_oim" "V_opg" "V_robust" "Vtype"
			  "converged"
			  "errorcode"
			  "gradient"
			  "iterationlog" "iterations"
			  "params"
			  "returncode"
			  "scores"
			  "value" "value0"
			) t ))
	   "("
	   )
	  '(1 ado-mata-function-name-face t) '(2 ado-mata-function-name-face t))

	  ;; obsolete mata functions
	(list
	 (concat
	  "\\<"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "acosr" "asinr" "atanr"
		 "ghkfastsetup"
		 "uniformseed"
		 ) t ))
	   "("
	   )
	  '(1 ado-obsolete-face))
	  
	  ;; all variable/macro stuff (put late so it will override)
	  ;; keep just before the obsolete commands!
	  ;; internal constants
	(list
	 (eval-when-compile 
	   (regexp-opt 
		'(
		 "_N" "_merge" "_n" "_pi" "_rc"
		 ) 'words))
	  '(1 ado-constant-face t))

	  ;; some generated vars
	  ;; ... which are now out of date
	(list
	 "\\<\\(_result([1-9]+[0-9]*)\\)"
	 '(1 ado-obsolete-face t))

	;; global macros
	(list
	   "\\([$][a-zA-Z_*]+[a-zA-Z_0-9]*\\)"
		'(1 ado-variable-name-face t))

	;; local macros
	;;   highlights *before* the macro is ended, which 
	;;   could lead to typos, but gets rid of recursive
	;;   definitions.
	(list
	 (concat
	  "`+\\(?:\\+\\+\\|--\\)?"
	  "\\([a-zA-Z_0-9]+\\)"
	  ) 
	 '(1 ado-variable-name-face t))
	
	(list
	  (concat
	   "`+"
	   "\\(\\(?:e\\|r\\|s\\)\\)"
	   "\\((\\)"
	   "\\([a-zA-Z_]+[a-zA-Z_0-9]*\\)"
	   "\\()\\)"
	   "'+"
	   )
	  '(1 ado-function-name-face t) '(2 ado-constant-face t)
	  '(3 ado-variable-name-face t) '(4 ado-constant-face t))

	  ;; obsolete mfp arguments
	  ;;  none --- looks to have been a documentation error

	  ;; what few obsolete commands I've gathered
	  ;; lfit and score moved before the matrix command so that it won't affect
	  ;; the matrix score command
	(list
	   (eval-when-compile 
		 (regexp-opt 
       '(
			 "_huber"
			 "adjust"
			 "aorder"
			 "archlm"
			 "bgodfrey"
			 "chdir" "cnr" "cnre" "cnreg" 
			 "dprobit" "ds" "durbina" "dwstat"
			 "ereg" "ereghet"
			 "gamma" "gammahet"
			 "gompertz" "gompertzhet"
			 "greigen" 
			 "iis" "impute" "ivreg"
			 "hettest"
			 "imtest"
			 "llogist" "llogistic" "llogistichet" "lnormal" "lnormalhet"
			 "lo" "loo" "look" "looku" "lookup"
			 "mfx"
			 "mov" "move" 
			 "nlinit"
			 "ovtest"
			 "lstat"
			 "poisgof"
			 "shelltool"
			 "stphtest" "svylc" "svytest" "szroeter"
			 "tis"
			 "varfcast" "varirf" "vce" "vif"
			 "weibull" "weibullhet"
			 "xtcorr" "xtrchh" "xthaus" "xtpois"
		 ) 'words))
	  '(1 ado-obsolete-face))

	  ;; the datasignature commands
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "datasig" "datasign" "datasigna" "datasignat" "datasignatu" "datasignatur" "datasignature"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"clear"
			"set"
			) 'words))
	   )
	  '(1 ado-builtin-harmful-face t) '(2 ado-subcommand-face t))
			  
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "datasig" "datasign" "datasigna" "datasignat" "datasignatu" "datasignatur" "datasignature"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			  "conf" "confi" "confir" "confirm" 
			  "rep" "repo" "repor" "report" 
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-subcommand-face t))

	  ;; the estat commands
	(list
	  (concat
	   "\\<\\(estat\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			  "abond" "alt" "alte" "alter" "altern" "alterna" "alternat" "alternati" "alternativ" "alternative" "alternatives"
			  "anova" "anti" "archlm" 
			  "bgo" "bgod" "bgodf" "bgodfr" "bgodfre" "bgodfrey"
			  "bootstrap"
			  "canontest" "clas" "class" "classfunctions" 
			  "classi" "classif" "classifi" "classific" 
			  "classifica" "classificat" "classificati" 
			  "classificatio" "classification" 
			  "classtable" 
			  "common" "compare"
			  "con" "conc" "conco" "concor" "concord" "concorda" "concordan" "concordanc" "concordance"
			  "config" "coordinates" 
			  "cor" "corr" "corre" "correl" "correla" "correlat" "correlati" "correlatio" "correlation"
			  "correlation" "correlations" 
			  "cov" "cova" "covar" "covari" "covaria" "covarian" "covarianc" "covariance" 
			  "cv"
			  "distances" 
			  "dur" "durb" "durbi" "durbin" "durbina" "durbinal" "durbinalt" 
			  "dwa" "dwat" "dwats" "dwatso" "dwatson" 
			  "eff" "effe" "effec" "effect" "effects" 
			  "endog" "endoge" "endogen" "endogeno" "endogenou" "endogenous" 
			  "errorrate"
			  "factors"
			  "facw" "facwe" "facwei" "facweig" "facweigh" "facweight" "facweights" 
			  "first" "firsts" "firstst" "firststa" "firststag" "firststage" 
			  "gof" "grdistances" "group" "grmeans" "grsummarize"
			  "hett" "hette" "hettes" "hettest" 
			  "ic" "imtest" "inertia"
			  "kmo"
			  "lceff" "lceffe" "lceffec" "lceffect" "lceffects" 
			  "list" "loadings"
			  "manova" "mfx" "mvreg"
			  "over" "overi" "overid" 
			  "ovtest"
			  "pairwise" "phtest" "predict" "profiles"
			  "quantiles"
			  "recovariance" "residuals" "rotate" "rotatecompare"
			  "sargan" "sd" "se" "size" "smc" "strata" "stress" "structure"
			  "su" "subinertia" "sum" "summ" "summa" "summar" 
			  "summari" "summariz" "summarize" 
			  "svyset"
			  "szroeter"
			  "table"
			  "vce" "vif"
			  "wcorrelation"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face))

	  ;; the estimates commands
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
			 "est" "esti" "estim" "estima" "estimat" "estimate" "estimates"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			 "clear"
			 "des" "desc" "descr" "descri" "describ" "describe"
			 "dir" 
			 "drop"
			 "esample"
			 "for"
			 "note" "notes" 
			 "q" "qu" "que" "quer" "query" 
			 "r" "re" 
			 "rep" "repl" "repla" "replay" 
			 "res" "rest" "resto" "restor" "restore" 
			 "save"
			 "stat" "stats" 
			 "sto" "stor" "store" 
			 "tab" "tabl" "table"
			 "title"
			 "use"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t))

    ;; the extra estimates notes commands
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
			 "est" "esti" "estim" "estima" "estimat" "estimate" "estimates"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			 "note" "notes"
			) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"drop"
			"l" "li" "lis" "list" 
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-subcommand-face t)
	  '(3 ado-subcommand-face t))

	;; things which are partially obsolete

	(list
	 (concat
	  "^[ \t]*"
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "jknife"
		  "parse"
		  "whelp"
		 ) 'words))
	   )
	  '(1 ado-obsolete-face))
    
	  ;; apparently obsolete window commands
	 (list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "win" "wind" "windo" "window"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "m" "me" "men" "menu"
			) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "popout"
		  "set"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	  '(3 ado-obsolete-face))

	 (list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		 "win" "wind" "windo" "window"
		 ) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "m" "me" "men" "menu"
			) 'words))
	   "[ \t]+"
	   "\\<\\(append\\)\\>"
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "popout"
		  "string"
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face) '(2 ado-builtin-harmless-face)
	  '(3 ado-obsolete-face t) '(4 ado-obsolete-face t))

	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"ma" "mac" "macr" "macro"
			) 'words))
	   "[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
			"de" "def" "define" 
			) 'words))
	   )
	  '(1 ado-builtin-harmful-face) '(2 ado-obsolete-face t))

	  ;; multiword extended macro names using 'set', which are obsolete
	(list
	  (concat
	   (eval-when-compile 
		 (regexp-opt 
       '(
		  "gl" "glo" "glob" "globa" "global" 
		  "loc" "loca" "local" 
		 ) 'words))
	   "[ \t]+"
	   "\\([a-zA-Z_]+[a-zA-Z_0-9]*\\)"
        "[ \t]*:[ \t]*"
		"\\<\\(set\\)\\>"
		"[ \t]+"
	   (eval-when-compile 
		 (regexp-opt 
		  '(
		  "adosize" "graphics" "level" "linesize" "logtype" "matsize" "more" "pagesize"
		  "rmsg" "trace" "type" "virtual" 
			) 'words))
	   )
	  '(1 ado-builtin-harmless-face t) '(2 ado-variable-name-face t)
	  '(3 ado-obsolete-face t) '(4 ado-obsolete-face t))
	)))

(require 'ado-scratch)

(provide 'ado-font-lock)