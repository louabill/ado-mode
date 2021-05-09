;;; ado-cons.el --- Setting constants for `ado-mode' -*- lexical-binding: t; package-lint-main-file: "ado-mode.el"; -*-

;; Copyright (C) 2020-2021 Bill Rising

;; Author:   Bill Rising <brising@alum.mit.edu>
;; Homepage: https://github.com/louabill/ado-mode

;; This file is not part of GNU Emacs.

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 3.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.
;; If not, see <https://www.gnu.org/licenses/>

;;; Commentary:

;;
;; This file contains constants for pulling apart Stata commands.
;; Perhaps these should be in ado-mode.el, but these constants could be
;;   used in multiple files.

;;; Code:
;;
;; Warning: there are many many close-parens on their own lines because
;;   this speeds up updating immensely


;; these regexps will still be fooled by line continuations
;;   or colons in the middle of a line of text
(defconst ado-capture-noisily-regexp
  (concat
   (eval-when-compile
	 (regexp-opt
	  '("cap" "capt" "captu" "captur" "capture")))
   "[ /t]+"
   (eval-when-compile
	 (regexp-opt
	  '("n" "no" "noi" "nois" "noisi" "noisil" "noisily"))))
  "The regexp for testing for a -capture noisily- prefix. Ugh.")


(defconst ado-prefix-any-regexp
  (concat
   "\\(?:[ \t]*"
   "\\(?:"
   (eval-when-compile
	 (regexp-opt
	  '(
		"cap" "capt" "captu" "captur" "capture"
		"mata"
		"n" "no" "noi" "nois" "noisi" "noisil" "noisily"
		"python"
		"qui" "quie" "quiet" "quietl" "quietly")))
   "\\|"
   ado-capture-noisily-regexp
   "\\)"
   "\\(?:[ \t]*:\\)?\\)?")
  "Regexp for commands which can precede another command without any colon.

Needed so that other command starters can include these more easily. What a pita.")

(defconst ado-start-cmd-regexp
  (concat
   "^\\(?:\\(?:.*:\\)*"
   "\\|"
   ado-prefix-any-regexp
   "\\)[ \t]*")
  "Start-of-command regexp to try to keep mid-line commands from highlighting.
Meant for typical commands which could allow a prefix command." )

(defconst ado-start-cmd-no-prefix-regexp
  (concat
   "^"
   ado-prefix-any-regexp
   "[ \t]*")
  "Start-of-command regexp for commands which do _not_ allow a prefix command." )
 
(defconst ado-start-cmd-must-start-line-regexp "^[ \t]*"
  "Start-of-command regexp for commands which don't allow capture, noisily, etc.
Meant for commands which do _not_ allow a prefix command.
Not implemented as much more than an experiment.")

(defconst ado-start-cmd-null-regexp ""
  "Empty start-of-command regexp to make clear that there is no leading regexp.
Meant for spurious-higlighting problems which have not been solved yet.")

(defconst ado-end-cmd-regexp "\\([ \t]+\\|,\\|;\\|:\\|$\\)"
  "End-of-command regexp to keep things like -regress(- from highlighting.")

(defconst ado-stata-name-wipe-bad-chars "[^[:space:][:cntrl:][-^`!-/:-@{-~]"
  "A attempt to pare out non-allowable characters in Stata names.")

(defconst ado-stata-name-regexp
  (concat "[[:alpha:]_]" ado-stata-name-wipe-bad-chars "*")
  "Regexp for uniform highlighting of Stata names.")

(defconst ado-stata-name-bound-regexp
  (concat "\\(" ado-stata-name-regexp "\\)")
  "Same as `ado-stata-name-regexp' bound within \\( and \\) for highlighting.")

(defconst ado-stata-varlist-start-regexp
  (concat "[[:alpha:]_*]" ado-stata-name-wipe-bad-chars "*")
  "Regexp for uniform highlighting of starts of varlists.")

(defconst ado-stata-name-varlist-start-regexp
  (concat "\\(" ado-stata-varlist-start-regexp "\\)")
  "Same as \\[ado-stata-varlist-start-regexp] but bound within \\( and \\)
for highlighting.")

;; (defconst ado-stata-local-name-regexp "[[:alnum:]_`][[:alnum:][:graph:]_`]*"
;; (defconst ado-stata-local-name-regexp "[[:alnum:]_`][^[:space:][:cntrl:][:punct:]]*"
(defconst ado-stata-local-name-regexp
  (concat "\\(?:`\\|" ado-stata-name-wipe-bad-chars "\\)"
		  ado-stata-name-wipe-bad-chars "*")
  "Regexp for highlighting local macros.")
  
(defconst ado-stata-local-name-bound-regexp
  (concat "\\(" ado-stata-local-name-regexp "\\)")
  "Regexp for highlighting local macros, bound within \\( and \\).")

(defconst ado-man-abbrevs '("BAYES"
							"D" "DSGE"
							"ERM"
							"FMM" "FN"
							"G" "GSM" "GSU" "GSW"
							"IG" "IRT"
							"M" "ME" "MI" "MV"
							"P" "PSS"
							"R"
							"SEM" "SP" "ST" "SVY"
							"TE" "TS"
							"U"
							"XT")
  "List of manual abbreviations, Stata-style.")

(provide 'ado-cons)

;;; ado-cons.el ends here
