;;; ado-mode.el --- Major mode for editing Stata-related files -*- lexical-binding: t; -*-

;; Copyright (C) 1996-2021 Bill Rising

;; Author: Bill Rising <brising@alum.mit.edu>
;; Version: 1.17.0.0
;; Keywords: tools,languages,files,convenience,Stata, Mata, ado
;; Package-requires: ((emacs "25.1"))
;; URL: https://github.com/louabill/ado-mode
;;
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

;; This package provides the ado-mode for smart highlighting and editing of
;; Stata, Mata, and SMCL code as well as the ability to send code to Stata and
;; open Stata help files easily.

;;; Change Log:

;; See the changes.txt file distributed with this package

;;; Code:

;;; required files
(require 'font-lock)				    ; for syntax highlighting
(require 'imenu)						; for jumping to subcommands
;;(require 'paragraphs)					; to shut up elint, but paragraphs.el
                                        ;  does not (provide 'paragraphs)?!
(require 'newcomment)				    ; to shut up elint
(require 'ado-cus)						; customization defs
(require 'ado-cons)                     ; constant defs for commands
(require 'ado-font)						; fontface defs
(require 'ado-clip)						; clipboard population
(require 'ado-to-stata)					; passing to Stata
(require 'ado-font-lock)				; font-lock definitions, all kazillion
(require 'ado-stata-info)				; gathering info from Stata
;;; Non-Emacs software also needed for sending code to Stata:
;;; Mac: nothing needed
;;; Win: AutoIt from https://www.autoitscript.com/site/autoit/downloads/
;;; *nix: xsel and xdotool, so for Ubuntu:
;;;   sudo apt-get install xsel xdotool

;;; putting in the proper extensions for using the ado-mode
;;; in reverse order because `add-to-list' adds to the front
(defvar ado-extensions '( "lbl" "class" "smcl" "dlg" "sthlp" "ihlp"
						  "hlp" "mata" "do" "ado"))
(setq ado-extensions (append (mapcar #'upcase ado-extensions) ado-extensions))
;; using add-to-list to keep dups from showing up; slow but reliable
;; a complicated way to make a loop
(dolist
	(exten
	 (mapcar (lambda(item) (cons (concat "\\." item "\\'") 'ado-mode))
			 ado-extensions))
  (add-to-list 'auto-mode-alist exten))

;; This variable is defined in ado-font-lock.el
;; (defvar ado-font-lock-keywords nil)
(defvar ado-font-lock-syntactic-keywords nil)
(defvar ado-extension nil)

;; abbrev table
(defvar ado-mode-abbrev-table nil
  "Abbrev table used while in ado mode.")
(define-abbrev-table 'ado-mode-abbrev-table ())

;; syntax table
(defvar ado-mode-syntax-table nil
  "Syntax table used while in ado mode.")

(unless ado-mode-syntax-table
  (setq ado-mode-syntax-table (make-syntax-table))
; (modify-syntax-entry ?\\ "." ado-mode-syntax-table) ;nullify escape meaning
  (modify-syntax-entry ?\\ "\\" ado-mode-syntax-table)
  (modify-syntax-entry ?\$ "." ado-mode-syntax-table)
  (modify-syntax-entry ?` "." ado-mode-syntax-table)
  (modify-syntax-entry ?\' "." ado-mode-syntax-table)
;  (modify-syntax-entry ?/ ". 124b" ado-mode-syntax-table)
  (modify-syntax-entry ?/ ". 14" ado-mode-syntax-table)
  (modify-syntax-entry ?* ". 23n" ado-mode-syntax-table)
;  (modify-syntax-entry ?\n "> b" ado-mode-syntax-table)
  (modify-syntax-entry ?+ "_" ado-mode-syntax-table)
  (modify-syntax-entry ?- "_" ado-mode-syntax-table)
  (modify-syntax-entry ?= "_" ado-mode-syntax-table)
  (modify-syntax-entry ?% "." ado-mode-syntax-table)
  (modify-syntax-entry ?< "." ado-mode-syntax-table)
  (modify-syntax-entry ?> "." ado-mode-syntax-table)
  (modify-syntax-entry ?& "." ado-mode-syntax-table)
  (modify-syntax-entry ?| "." ado-mode-syntax-table)
  (modify-syntax-entry ?~ "." ado-mode-syntax-table)
  ;; added this, because underscores really are parts of words
  (modify-syntax-entry ?_ "w" ado-mode-syntax-table)
  ;; an attempt to fix embedded quote highlighting problems
  ;; fails because string starts and ends can be only single characters
;  (modify-syntax-entry ?` "| 1b" ado-mode-syntax-table)
;  (modify-syntax-entry ?' "| 4b" ado-mode-syntax-table)
;  (modify-syntax-entry ?\" "| 23b" ado-mode-syntax-table)
  )

;;; keymap definitions
;;;   put in `let' on advice from MELPA
(defvar ado-mode-map
  (let ((kmap (make-sparse-keymap)))
	(define-key kmap "\t"       'ado-indent-line)
	(define-key kmap [(meta return)] 'ado-send-command-to-stata)
	(define-key kmap [(meta control return)] 'ado-send-buffer-to-stata)
	(define-key kmap [(meta shift return)] 'ado-split-line)
	(define-key kmap "\C-c\C-a"   'ado-mode)
	(define-key kmap "\C-c\C-b"   'ado-grab-block)
	(define-key kmap "\C-c\M-b"   'ado-send-block-to-stata)
	(define-key kmap "\C-c\C-d"   'ado-new-help)
	(define-key kmap "\C-c\C-e"   'ado-foreach-loop)
	(define-key kmap "\C-c\C-f"   'ado-open-any-file)
	(define-key kmap "\C-c\M-f"   'ado-open-command)
	(define-key kmap "\C-c\C-k"   'ado-help-at-point)
	(define-key kmap "\C-c\M-k"   'ado-help-command)
	(define-key kmap "\C-c\C-i"   'ado-insert-new-program)
	(define-key kmap "\C-c\C-j"   'ado-strmacify-selection-or-word)
	(define-key kmap "\C-c\C-l"   'ado-new-label)
	(define-key kmap "\C-c\C-m"   'ado-macify-selection-or-word)
	(define-key kmap "\C-c\C-n"   'ado-new-program)
	(define-key kmap "\C-c\C-o"   'ado-open-any-file)
	(define-key kmap "\C-c\M-o"   'ado-open-command)
	(define-key kmap "\C-c\C-s"   'ado-stringify-selection)
	(define-key kmap "\C-c\C-t"   'ado-input-to-stata)
	(define-key kmap "\C-c\C-v"   'ado-forvalues-loop)
	(define-key kmap "\M-a"       'ado-beginning-of-command)
	(define-key kmap "\M-e"       'ado-end-of-command)
	;;(define-key kmap "\C-c;"    'comment-region)
	;;(define-key kmap "\C-c:"    'uncomment-region)
	;; finally not needed anymore!
	;;(define-key kmap "\C-x\C-s"   'ado-save-program)
	(define-key kmap "{"          'ado-electric-brace)
	(define-key kmap "}"          'ado-electric-closing-brace)
	(define-key kmap ";"          'ado-electric-semi)

;;; menu bar definitions
	(define-key kmap [menu-bar] (make-sparse-keymap))
	(define-key kmap [menu-bar ado]
	  (cons "Ado-mode" (make-sparse-keymap "Ado-mode")))
	(define-key kmap [menu-bar ado options]
	  (cons "Options" (make-sparse-keymap "options")))
	(define-key kmap [menu-bar ado l4]
	  '(menu-item "--single-line"))
	(define-key kmap [menu-bar ado ado-open-command]
	  '("Open command" . ado-open-command))
	(define-key kmap [menu-bar ado ado-open-any-file]
	  '("Open Stata file" . ado-open-any-file))
	(define-key kmap [menu-bar ado ado-help-command]
	  '("Help for command" . ado-help-command))
	(define-key kmap [menu-bar ado ado-help-at-point]
	  '("Help at point" . ado-help-at-point))
	(define-key kmap [menu-bar ado l3]
	  '(menu-item "--single-line"))
	(define-key kmap [menu-bar ado indent-buffer]
	  '("Indent Region" . ado-indent-region))
	(define-key kmap [menu-bar ado indent-buffer]
	  '("Indent Buffer" . ado-indent-buffer))
	(define-key kmap [menu-bar ado new]
	  (cons "New program" (make-sparse-keymap "new")))
	(define-key kmap [menu-bar ado l2]
	  '(menu-item "--single-line"))
	;;(define-key kmap [menu-bar ado uncomment-region]
	;;  '("Uncomment Region" . uncomment-region))
	;;(define-key kmap [menu-bar ado comment-region]
	;;  '("Comment Out Region" . comment-region))
	(define-key kmap [menu-bar ado ado-forvalues-loop]
	  '("Forvalues loop" . ado-forvalues-loop))
	(define-key kmap [menu-bar ado ado-foreach-loop]
	  '("Foreach loop" . ado-foreach-loop))
	(define-key kmap [menu-bar ado l1]
	  '(menu-item "--single-line"))
	
;; place for customizations
	(define-key kmap [menu-bar ado strmacify-word]
	  '("Stringify and Macify Word or Selection" . ado-strmacify-selection-or-word))
	(define-key kmap [menu-bar ado stringify-selection]
	  '("Stringify Selection" . ado-stringify-selection))
	(define-key kmap [menu-bar ado macify-word]
	  '("Macify Word or Selection" . ado-macify-selection-or-word))
	(define-key kmap [menu-bar ado l0]
	  '(menu-item "--single-line"))
	(define-key kmap [menu-bar ado ado-end-of-command]
	  '("Go to end of command" . ado-end-of-command))
	(define-key kmap [menu-bar ado ado-beginning-of-command]
	  '("Go to beginning of command" . ado-beginning-of-command))
	(define-key kmap [menu-bar ado l_1]
	  '(menu-item "--single-line"))
	(define-key kmap [menu-bar files save-buffer]
	  '("Save buffer" . ado-save-program))
	(define-key kmap [menu-bar ado save-program]
	  '("Save buffer" . ado-save-program))

;; submenu New
	(define-key kmap [menu-bar ado new ado-new-label]
	  '("Label file" . ado-new-label))
	(define-key kmap [menu-bar ado new ado-new-do]
	  '("Do-file" . ado-new-do))
	(define-key kmap [menu-bar ado new ado-new-help]
	  '("Help file" . ado-new-help))
	(define-key kmap [menu-bar ado new ado-new-cscript]
	  '("Cert script" . ado-new-cscript))
	(define-key kmap [menu-bar ado new ado-new-testado]
	  '("New do-file for program testing" . ado-new-testado))
	(define-key kmap [menu-bar ado new ado-insert-new-program]
	  '("Insert new subprogram" . ado-insert-new-program))
	(define-key kmap [menu-bar ado new ado-new-program]
	  '("Generic new program" . ado-new-program))
	
	;; submenu Options
;;; this submenu follows
	(define-key kmap [menu-bar ado options ado-stata-interaction-menu]
	  (cons "Ado-Stata Interaction" (make-sparse-keymap "ado-data-interaction-menu")))

	(define-key kmap [menu-bar ado options special-indentation]
	  (cons "Special Indentation" (make-sparse-keymap "special-indentation")))

	(define-key kmap [menu-bar ado options ado-help-author-toggle]
	  '(menu-item "Include Author section in help files"
				  (lambda ()
					(interactive) (ado-toggle-flag 'ado-help-author-flag))
				  :button (:toggle . ado-help-author-flag)))

	(define-key kmap [menu-bar ado options ado-comeback-toggle]
	  '(menu-item "Return to Emacs after submission"
				  (lambda ()
					(interactive) (ado-toggle-flag 'ado-comeback-flag))
				  :button (:toggle . ado-comeback-flag)))

	(define-key kmap [menu-bar ado options ado-open-read-only-toggle]
	  '(menu-item "Open files from adopath in read-only mode"
				  (lambda ()
					(interactive) (ado-toggle-flag 'ado-open-read-only-flag))
				  :button (:toggle . ado-open-read-only-flag)))
	
	(define-key kmap [menu-bar ado options ado-confirm-overwrite-toggle]
	  '(menu-item "Confirm File Overwrite"
				  (lambda ()
					(interactive) (ado-toggle-flag 'ado-confirm-overwrite-flag))
				  :button (:toggle . ado-confirm-overwrite-flag)))

	(define-key kmap [menu-bar ado options ado-comment-column-change]
	  '(menu-item "Set Comment Column..."
				  (lambda ()
					(interactive) (ado-change-number 'ado-comment-column 'ask))))

	(define-key kmap [menu-bar ado options ado-continued-statement-indent-spaces-change]
	  '(menu-item "Set Continuation Indentation..."
				  ado-continued-statement-indent-spaces-change))

	(define-key kmap [menu-bar ado options ado-tab-width-change]
	  '(menu-item "Set Tab Width..." ado-tab-width-change))

	(define-key kmap [menu-bar ado options ado-update-timestamp-toggle]
	  '(menu-item "Update Timestamps on Save"
				  (lambda ()
					(interactive) (ado-toggle-flag 'ado-update-timestamp-flag))
				  :button (:toggle . ado-update-timestamp-flag)))

	(define-key kmap [menu-bar ado options ado-fontify-new-toggle]
	  '(menu-item "Fontify New Ado Files"
				  (lambda ()
					(interactive) (ado-toggle-flag 'ado-fontify-new-flag))
				  :button (:toggle . ado-fontify-new-flag)))

	(define-key kmap [menu-bar ado options ado-auto-newline-toggle]
	  '(menu-item "Automatic New Line"
				  (lambda ()
					(interactive) (ado-toggle-flag 'ado-auto-newline-flag))
				  :button (:toggle . ado-auto-newline-flag)))

	(define-key kmap [menu-bar ado options ado-closing-brace-alone-toggle]
	  '(menu-item "Closing Brace Alone"
				  (lambda ()
					(interactive) (ado-toggle-flag 'ado-closing-brace-alone-flag))
				  :button (:toggle . ado-closing-brace-alone-flag)))

	(define-key kmap [menu-bar ado options ado-close-under-line-toggle]
	  '(menu-item "Close Under Line"
				  (lambda ()
					(interactive) (ado-toggle-flag 'ado-close-under-line-flag))
				  :button (:toggle . ado-close-under-line-flag)))

	(define-key kmap [menu-bar ado options ado-use-modern-split-toggle]
	  '(menu-item "Use Modern Line-split"
				  (lambda ()
					(interactive) (ado-toggle-flag 'ado-use-modern-split-flag))
				  :button (:toggle . ado-use-modern-split-flag)))

	(define-key kmap [menu-bar ado options ado-do-indent-toggle]
	  '(menu-item "Indent Do Files"
				  (lambda ()
					(interactive) (ado-toggle-flag 'ado-do-indent-flag))
				  :button (:toggle . ado-do-indent-flag)))

;; needs its own toggling function, because keymaps must be changed.
	(define-key kmap [menu-bar ado options ado-return-also-indents-toggle]
	  '(menu-item "Return also Indents" ado-return-toggle
				  :button (:toggle . ado-return-also-indents-flag)))

	(define-key kmap [menu-bar ado options ado-smart-indent-toggle]
	  '(menu-item "Smart Indent"
				  (lambda ()
					(interactive) (ado-toggle-flag 'ado-smart-indent-flag))
				  :button (:toggle . ado-smart-indent-flag)))

;; subsubmenu Options/Special Indent
	(define-key kmap [menu-bar ado options special-indentation ado-change-comment-indent]
	  '(menu-item "Change comment indent column..."
				  (lambda ()
					(interactive) (ado-change-number 'ado-comment-indent-column 'ask))
				  :enable ado-delimit-indent-flag))

	(define-key kmap [menu-bar ado options special-indentation ado-comment-indent-flag-toggle]
	  '(menu-item "Comment column indentation"
				  (lambda ()
					(interactive) (ado-toggle-flag 'ado-comment-indent-flag))
				  :button (:toggle . ado-comment-indent-flag)))

	(define-key kmap [menu-bar ado options special-indentation ado-change-delimit-indent]
	  '(menu-item "Change #delimit column..."
				  (lambda ()
					(interactive) (ado-change-number 'ado-delimit-indent-column 'ask))
				  :enable ado-delimit-indent-flag))

	(define-key kmap
	  [menu-bar ado options special-indentation ado-delimit-indent-flag-toggle]
	  '(menu-item "#delimit indented differently"
				  (lambda ()
					(interactive) (ado-toggle-flag 'ado-delimit-indent-flag))
				  :button (:toggle . ado-delimit-indent-flag)))

	(define-key kmap
	  [menu-bar ado options special-indentation ado-change-debugging-indent]
	  '(menu-item "Change debugging column..."
				  (lambda ()
					(interactive) (ado-change-number 'ado-debugging-indent-column 'ask))
				  :enable ado-debugging-indent-flag))

	(define-key kmap
	  [menu-bar ado options special-indentation ado-debugging-indent-flag-toggle]
	  '(menu-item "debugging indented differently"
				  (lambda ()
					(interactive) (ado-toggle-flag 'ado-debugging-indent-flag))
				  :button (:toggle . ado-debugging-indent-flag)))

;; subsubmenu Options/Special Indent
	(define-key kmap
	  [menu-bar ado options ado-stata-interaction-menu ado-send-instance-set]
	  '(menu-item "Set Stata instance..."
				  (lambda ()
					(interactive) (ado-change-number 'ado-stata-instance 'ask))))

	(define-key kmap [menu-bar ado options ado-stata-interaction-menu ado-send-flavor-set]
	  '(menu-item "Set Stata flavor..."
				  (lambda ()
					(interactive)
					(setq ado-stata-flavor
						  (read-from-minibuffer "Change ado-stata-version to (blank for any) ")))))

	(define-key kmap [menu-bar ado options ado-stata-interaction-menu ado-send-version-set]
	  '(menu-item "Set Stata version..."
				  (lambda ()
					(interactive)
					(setq ado-stata-version
						  (read-from-minibuffer "Change ado-stata-version to (blank for any) ")))))

	(define-key kmap
	  [menu-bar ado options ado-stata-interaction-menu ado-send-to-all-toggle]
	  '(menu-item "Send code to all matching Statas"
				  (lambda ()
					(interactive) (ado-toggle-flag 'ado-send-to-all-flag))
				  :button (:toggle . ado-send-to-all-flag)))

	(define-key kmap
	  [menu-bar ado options ado-stata-interaction-menu ado-strict-match-toggle]
	  '(menu-item "Use strict matching for finding Stata instance"
				  (lambda () (interactive) (ado-toggle-flag 'ado-strict-match-flag))
				  :button (:toggle . ado-strict-match-flag)))
	kmap)
	"Keymap for `ado-mode'." )



;; initial mode defintion function
(defun ado-mode ()
  "Major mode for editing files associated with the Stata statistical package.

It can be used to edit ado, do, mata, sthlp, hlp, dlg, and smcl files while
indenting blocks of code properly, highlighting command names, (most) keywords,
more-complicated command structures, strings, Stata macro names and the like.

Ado-mode comes with a menu (the Ado-mode menu) which shows most all of the
variables which are worth changing locally in a buffer. Global customization
can be done via '\\[customize-group] ado' using Emacs customization
routines. More suggestions can be found at
https://louabill.org/Stata/ado-mode_install.html

Here is a short list of the common commands which come with the mode:
Most helpful things
- `ado-new-ado' makes a new buffer ready for a new ado file.
- `ado-new-do' makes a buffer ready for a well-logged do file.
- `ado-new-help' starts a new help file, ready for editing.
- `ado-new-mata' starts a new mata file.
- `ado-new-label' will make a new label file useful for storing commonly
    used value labels.
- `ado-mode' can interact directly with Stata:
    \\[ado-send-command-to-stata] will send the current selection
      to Stata for evaluation. If nothing is selected, the command containing
      the insertion bar will be sent.
    \\[ado-help-command] gets Stata help on the current command---even if you
      are in the middle of a long command.
    \\[ado-help-at-point] gets Stata help for the word at point.

Saving a buffer will save the current buffer and give it a good
  timestamp (if the `ado-update-timestamp-flag' is true, which it is
  by default). It will also do its best to ensures that the file name
  matches the name of the command (ado program) or class being defined.
    
Fixing up indentation:
- `ado-indent-buffer' will re-indent the whole buffer.

Things for special Stata manipulations
- \\[ado-beginning-of-command] will move the point back to the beginning
    of the current command. If in the whitespace between two commands, it will
    move to the start of the next command.
- \\[ado-split-line] will split a long line in two using either the /* */ or ///
    style comments, depending on the value of `ado-use-modern-split-flag' (which
    defaults to on, implying the use of ///).
- \\[ado-foreach-loop] will insert a foreach loop, asking in the minibuffer
    for the particulars.
- \\[ado-forvalues-loop] will insert a forvalues loop, asking in the minibuffer for the particulars.
- `ado-insert-new-program' puts a new subprogram at the bottom of the current
    buffer for use within the current ado file.
- \\[ado-macify-selection-or-word] will turn the current selection or the word containing the point into a local macro.
- \\[ado-strmacify-selection-or-word] will turn the current selection or the word containing the point into a local macro enclosed in full string qualification.
- \\[ado-stringify-selection] will enclose the current selection with full string qualification.

Most all of the commands are accessible from the Ado-mode menu.

If you also use ESS (Emacs Speaks Statistics), but you would rather
use this `ado-mode' to code Stata, include the following in your .emacs
file:

 (setq auto-mode-alist
      (append (list '(\"\\\\.ado\\\\'\" . ado-mode)
		    '(\"\\\\.do\\\\'\"  . ado-mode)
		    )
	      auto-mode-alist))

This will make ado-mode load when you open an ado or do file.

Finally, here is the complete keymap for ado-mode:

\\{ado-mode-map}"
;; standard variables for any mode
  (interactive)
  (kill-all-local-variables)
;; testing...doesn't seem to have an effect
;;  (setq syntax-begin-function 'ado-beginning-of-command)
  (use-local-map ado-mode-map)
  (define-abbrev-table 'ado-mode-abbrev-table ())
  (setq local-abbrev-table ado-mode-abbrev-table)
  (set-syntax-table ado-mode-syntax-table)
  (make-local-variable 'ado-return-also-indents-flag)
  (ado-set-return ado-return-also-indents-flag)
  ;; indentation and paragraph definitions
  (make-local-variable 'indent-line-function)
  (setq indent-line-function #'ado-indent-line)
  (make-local-variable 'paragraph-start)
;; changed June 26, 2015 to same def as from c-mode
  (setq paragraph-start (concat "[ 	]*\\(//\\|///\\|\\**\\)[ 	]*$\\|^" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  ;; comment definitions
  (make-local-variable 'comment-start)
  (setq comment-start "/* ")
  (make-local-variable 'comment-end)
  (setq comment-end " */")
  (make-local-variable 'comment-column)
  (setq comment-column ado-comment-column)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+ *")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'ado-comment-indent)
  (make-local-variable 'comment-multi-line)
  (setq comment-multi-line nil)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  ;; make local copies of all the style stuff
  (make-local-variable 'ado-smart-indent-flag)
  (make-local-variable 'ado-comment-column)
  (make-local-variable 'ado-tab-width)
  (setq tab-width ado-tab-width)
  (make-local-variable 'ado-confirm-overwrite-flag)
  (make-local-variable 'ado-use-modern-split-flag)
  (make-local-variable 'ado-delimit-indent-flag)
  (make-local-variable 'ado-delimit-indent-column)
  (make-local-variable 'ado-comment-indent-flag)
  (make-local-variable 'ado-comment-indent-column)
  (make-local-variable 'ado-debugging-indent-flag)
  (make-local-variable 'ado-debugging-indent-column)
  (make-local-variable 'ado-fontify-new-flag)
  (make-local-variable 'ado-close-under-line-flag)
  (make-local-variable 'ado-continued-statement-indent-spaces)
  (make-local-variable 'ado-auto-newline-flag)
  (make-local-variable 'ado-closing-brace-alone-flag)
  (make-local-variable 'ado-comeback-flag)
  (make-local-variable 'ado-submit-default)
  (make-local-variable 'ado-stata-instance)
  (make-local-variable 'ado-stata-flavor)
  (make-local-variable 'ado-stata-version)
  ;; delete auto-save-file when file is saved for real
  (make-local-variable 'delete-auto-save-files)
  (setq delete-auto-save-files t)
  (make-local-variable 'before-save-hook)
  (add-hook 'before-save-hook ado-before-save-file-hook)
  (use-local-map ado-mode-map)
  (setq mode-name "Ado")
  (setq major-mode 'ado-mode)
  ;; make sure files end with lf because of Stata's parser
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'font-lock-defaults)
  (ado-set-font-lock-keywords)
  (setq font-lock-defaults '(ado-font-lock-keywords))
  (make-local-variable 'font-lock-syntactic-keywords)
  ;; make local copy of the extension, and try to guess the extension
  (make-local-variable 'ado-extension)
  (setq ado-extension (ado-find-extension))
  ;; setup directories which could be needed
  ;;  Melpa insists that all .el files must be copied up one directory, which
  ;;    messes with any decent file structure
  (unless ado-mode-home
	(setq ado-mode-home
		  (file-name-as-directory (expand-file-name (file-name-directory (locate-file "ado-mode.el" load-path))))))
  ;; test to see if scripts is in ado-mode-home
  ;;   it will be if from MELPA, will not be if from github
  ;;   if scripts is not in ado-mode-home, then ado-mode-home needs to be fixed
  (unless (file-exists-p (file-name-as-directory (concat ado-mode-home "scripts")))
	(setq ado-mode-home
		  (file-name-as-directory
		   (expand-file-name (concat (file-name-as-directory ado-mode-home) "..")))))
	(unless ado-site-template-dir
	(setq ado-site-template-dir (file-name-as-directory (concat ado-mode-home "templates"))))
  (unless ado-script-dir
	(setq ado-script-dir (file-name-as-directory (concat ado-mode-home "scripts"))))
  (unless ado-new-dir
	(unless ado-personal-dir
	  (ado-reset-personal-dir))
	(setq ado-new-dir ado-personal-dir))
  
  (if ado-smart-indent-flag
      (if (or
			  (string= ado-extension "hlp")
			  (string= ado-extension "sthlp")
			  (string= ado-extension "dlg")
			  (string= ado-extension "idlg"))
			 (setq ado-smart-indent-flag nil)
		  (if (string= ado-extension "do")
				(setq ado-smart-indent-flag ado-do-indent-flag))))
  (when ado-add-sysdir-font-lock
	(ado-add-sysdir-all))
  (run-mode-hooks 'ado-mode-hook))

;;; ado-set-return == t -> swap ret and C-j
(defun ado-set-return (state)
  "Set the bindings of `C-m' and `C-j'.
STATE nil: standard Emacs behavior where `C-j' is like `newline-and-indent'.
STATE t: better behavior to have `C-m' like `newline-and-indent'."
  (if state
      (progn
		(define-key ado-mode-map "\C-m" 'ado-newline)
		(define-key ado-mode-map "\C-j" 'newline))
    (define-key ado-mode-map "\C-j" 'ado-newline)
    (define-key ado-mode-map "\C-m" 'newline)))

;;;; all the style-toggles for local resets rather than global

(defun ado-return-toggle ()
  "Toggle the state of the `RET' key."
  (interactive)
  (setq ado-return-also-indents-flag (not ado-return-also-indents-flag))
  (ado-set-return ado-return-also-indents-flag))

(defun ado-toggle-flag (flag-name)
  "Generic function for toggling FLAG-NAME."
  (interactive "vWhat flag would you like to toggle? ")
  (set flag-name (not (eval flag-name))))

;;;; all the style value changers for local changes
;; a function which makes all the prompts and messages look the same.
;; returns nil if nothing changed, or t if there is a change
(defun ado-change-number (variable newvalue)
  "Interface for changing options which have numerical values somewhat nicely.
Does not work if fed an expression.
VARIABLE is for the variable name.
NEWVALUE is for the newvalue."
  (interactive "vWhat variable would you like to change? \ni")
  (if (or (null newvalue)
		  (eq newvalue 'ask))
      (progn
		(setq newvalue (read-from-minibuffer (concat "Change " (symbol-name variable) " to ")  (number-to-string (eval variable) )))
		(if (or
			 (string= newvalue "")
			 (= (setq newvalue (string-to-number newvalue)) (eval variable)))
			(progn
			  (message "Value of `%s' left unchanged." (symbol-name variable))
			  nil)
		  (set variable (eval newvalue))
		  (message "Value of `%s' set to %d." (symbol-name variable) (eval variable))
		  t))
    (set variable (eval newvalue))
    t ))

(defun ado-tab-width-change (&optional new-tab-width)
  "Change `tab-width' for the current buffer, optionally re-indent.
NEW-TAB-WIDTH is an optional argument; when left nil, the user gets prompted
for a new value."
  (interactive)
  (when (ado-change-number 'tab-width new-tab-width)
    (when (y-or-n-p "Reindent buffer now? ")
	  (save-excursion)
	  (ado-indent-buffer))))

(defun ado-continued-statement-indent-spaces-change (&optional spaces)
  "Change the number of spaces for continued commands, and optionally re-indent.
SPACES, when non-nil, indents using spaces instead of tab characters."
  (interactive)
  (when (ado-change-number 'ado-continued-statement-indent-spaces spaces)
    (when (y-or-n-p "Reindent buffer now? ")
	  (save-excursion)
	  (ado-indent-buffer))))

;;; scunged from the c-mode indentation
(defun ado-comment-indent ()
  "Indent line-starting comments.
Stolen from `c-mode' indention."
  (if (looking-at "^/\\*")
      0				;Existing comment at bol stays there.
    (let ((opoint (point)))
      (save-excursion
		(beginning-of-line)
		(cond ((looking-at "[ \t]*}[ \t]*\\($\\|/\\*\\)")
			   ;; A comment following a solitary close-brace
			   ;; should have only two spaces.
			   (search-forward "}")
			   (+ 2 (current-column)))
			  ((or (looking-at "^#[ \t]*endif[ \t]*")
				   (looking-at "^#[ \t]*else[ \t]*"))
			   7)			;2 spaces after #endif
			  ((progn
				 (goto-char opoint)
				 (skip-chars-backward " \t")
				 (and (= comment-column 0) (bolp)))
			   ;; If comment-column is 0, and nothing but space
			   ;; before the comment, align it at 0 rather than 1.
			   0)
			  (t
			   (max (1+ (current-column))	;Else indent at comment column
					comment-column)))))))	; except leave at least one spaces.

(defun ado-continuation-indent ()
  "Indent continuation characters to `comment-column'.
If there is no continuation on the current line, inserts the proper
continuation characters."
  (interactive)
  (let (cont-string
		cont-length
		(found-it t))
	(if ado-use-modern-split-flag
		(setq cont-string "///")
	  (setq cont-string "/*"))
	(setq cont-length (length cont-string))
	(end-of-line)
	(skip-chars-backward " \t")
	(if ado-use-modern-split-flag
		(if (re-search-backward "[ \t]+///" (point-at-bol) t)
			(skip-chars-forward " \t")
		  (setq found-it nil))
	  (if (and (re-search-backward "[ \t]+/\\*" (point-at-bol) t)
			   (not (re-search-forward "\\*/" (point-at-eol) t)))
		  (skip-chars-forward " \t")
		(setq found-it nil)
		(end-of-line)))
	(unless found-it
	  (unless (looking-back "[ \t]+" (point-at-bol))
		(insert " "))
	  (insert cont-string)
	  (forward-char (- cont-length)))
	(indent-to-column (max (1+ (current-column)) comment-column))
	(forward-char cont-length)))
	 

;; useful things which are better than keyboard macros
(defun ado-parse-loop ()
  "Out-of-date helper function. Use \\[ado-foreach-loop] instead."
  (interactive)
  (error "This is out of date! Use a foreach loop (\\[ado-foreach-loop]), instead"))

(defun ado-foreach-loop (&optional macname listtype)
  "Insert a foreach loop, after asking for the type of loop to insert.
The optional first argument MACNAME is the name for the local macro
  to hold tokens.
The optional second argument LISTTYPE is thethe type of list to be parsed.
When either is unspecified, the user gets prompted for values."
  (interactive)
  (unless macname
	(setq macname (read-from-minibuffer "What local macro should hold the tokens? ")))
  (unless listtype
	(setq listtype (read-from-minibuffer "What type of list? (leave blank if not special) ")))
  (insert (concat "foreach " macname))
  (if (equal listtype "")
      (insert " in \"\"")
    (insert (concat " of " listtype "  ")))
  (ado-insert-with-lfd " {")
  (newline-and-indent)
  (insert "}")
  (search-backward "{")
  (backward-char 2))

(defun ado-forvalues-loop (&optional macname range)
  "Insert a forvalues loop, after asking for the range to insert.
The optional first argument MACNAME holds the local macro name.
The optional second argument RANGE holds the Stata start/stop-style range."
  (interactive)
  (unless macname
	(setq macname (read-from-minibuffer "What local macro should hold the tokens? ")))
  (while (not range)
	(setq range (read-from-minibuffer "What numlist? ")))
  (ado-indent-line)
  (insert (concat "forvalues " macname " = " range ))
  (ado-insert-with-lfd " {")
  (ado-indent-line)
  (save-excursion
	(newline-and-indent)
	(insert "}")))

(defun ado-new-generic (type exten &optional stayput name purpose cusblp)
  "Generic file-creator for easing creation of specific files.

This makes creating new ado, class, do, mata and other files simpler by
having a standard programminginterface.

TYPE specifies the type of file. Current choices are
  program - for do- or ado-fileds
  do-file - for do-files
  ado     - for ado-files
  class   - for ado (non-mata) class files
  hlp     - for deprecated help files (deprecated in Stata 7)
EXTEN specifies the file extension.

STAYPUT, when non-nil means to save the file in the current directory.
NAME specifies the name of the file, excluding the extension.
PURPOSE specifies the purpose of the file, which will be put in a *! comment
  at the top of the file.
CUSBLP specifies a custom template file. When not specified, the template will
  come from the templates which come with `ado-mode' and will be chosen
  based on the TYPE and/or EXTEN."
  (let (fullname buffullname (keepbuf t) (searchstr "startHere"))
	(unless name
	  (setq name (read-from-minibuffer
				  (concat "What is the name of the " type "? "))))
	(setq fullname (concat name "." exten))
	(setq buffullname
		  (set-buffer (generate-new-buffer fullname)))
	(ado-mode)
	(if cusblp
		(ado-insert-boilerplate cusblp nil t)
	  (ado-insert-boilerplate
	   (if (and (string= type "program")
				(string= exten "do"))
		   "testado.blp"
		 (concat exten ".blp"))))
	(unless purpose
	  (goto-char (point-min))
	  (if (search-forward "*!")
		  (setq purpose (read-from-minibuffer "What does it do? "))))
	(if (and (or ado-new-dir ado-personal-dir)
			 (not stayput)
			 (not (string= type "do-file")))
		(if ado-new-dir
			(if (y-or-n-p "Put in 'new' directory? ")
				(cd (directory-file-name ado-new-dir)))
		  (if ado-personal-dir
			  (if (y-or-n-p "Put in 'personal' directory? ")
				  (cd (directory-file-name ado-personal-dir))))))
	(if (file-exists-p fullname)
		(setq keepbuf (y-or-n-p (concat "File " fullname " already exists! Overwrite?"))))
	(if keepbuf
		;; need progn because of else way far down
		(progn
		  (if (string= ado-version-command "")
			  (ado-reset-version-command))
		  (goto-char (point-min))
		  (while (search-forward "stata!!version" nil t)
			(replace-match ado-version-command))
		  (goto-char (point-min))
		  (when (search-forward "*!")
			(end-of-line)
			(insert (ado-nice-current-date))
			(when (search-forward "*!")
			  (end-of-line)
			  (insert purpose)))
		  (goto-char (point-min))
		  (while (search-forward "putNameHere" nil t)
			(replace-match name))
		  (goto-char (point-min))
		  (while (search-forward searchstr nil t)
			(replace-match ""))
		  (if ado-fontify-new-flag (turn-on-font-lock))
		  ;; .ado, .class, and the outdated .hlp files are the only ones where
		  ;;     a decent name can be made
		  (switch-to-buffer buffullname)
		  (if (or
			   (string= type "ado")
			   (string= type "class")
			   (string= type "hlp"))
			  (ado-save-program)
			(set-visited-file-name (concat name "." exten))
			(ado-save-program))) ;; end for keepbuf true
	  (kill-buffer buffullname))))

(defun ado-new-do (&optional stayput name purpose)
  "Make a new do-file in the current directory.
The optional arguments STAYPUT, NAME, and PURPOSE get fed to \\[ado-new-generic].

When used interactively, you'll get asked for the name of the file
  and its purpose.

The default do-file is made to keep its own named log so that it can be
called by other do-files with log-files.

Bound to \\[ado-new-do]."
  (interactive)
  (ado-new-generic "do-file" "do" stayput name purpose))

(defun ado-new-mata (&optional stayput name purpose)
  "Make a new mata file either in the current or `new' directory.
The optional arguments STAYPUT, NAME, and PURPOSE get fed to `ado-new-generic'.

Used interactively, you'll get asked for the name of the file and its purpose,
and where the file should be saved.

You can change the default mata-file by altering the mata.blp file.

Bound to \\[ado-new-mata]"
  (interactive)
  (ado-new-generic "mata file" "mata" stayput name purpose))

(defun ado-new-class (&optional stayput name purpose)
  "Make a new class file either in the current or `new' directory.
The optional arguments STAYPUT, NAME, and PURPOSE get fed to \\[ado-new-generic].

Used interactively, you'll get asked for the name of the file and its purpose,
and where the file should be saved.

Bound to \\[ado-new-class]"
  (interactive)
  (ado-new-generic "class" "class" stayput name purpose))

(defun ado-new-program (&optional stayput name purpose)
  "Make a new ado-file either in the current or `new' directory.
The optional arguments STAYPUT, NAME, and PURPOSE get fed to \\[ado-new-generic].

Used interactively, you'll get asked for the name of the file and its purpose,
and where the file should be saved.

Bound to \\[ado-new-ado]"
  (interactive)
  (ado-new-generic "program" "ado" stayput name purpose))

(defalias 'ado-new-ado #'ado-new-program)

(defun ado-new-testado (&optional stayput name purpose)
  "Make a new ado file together with a do-file for testing.
The optional arguments STAYPUT, NAME, and PURPOSE get fed to \\[ado-new-generic].

Used interactively, you'll get asked for the name of the file and its purpose,
and where the file should be saved.

By default, `ado-new-testado' creates a do-file which  -includes- an ado-file
by the same name for easier debugging.

Bound to \\[ado-new-testado]"
  (interactive)
  (ado-new-generic "program" "do" stayput name purpose))
  
(defun ado-insert-new-program (&optional name)
  "Insert a subprogram at the bottom of the current buffer.
The optional argument NAME is the name of the subprogram.

Used interactively, you'll get asked for the name of the program.

Bound to \\[ado-insert-new-program]"
  (interactive)
  (unless name
	(setq name (read-from-minibuffer "What is the name of the program? ")))
  (goto-char (point-max))
  (ado-insert-boilerplate "smallado.blp")
  (search-forward "program define")
  (end-of-line)
  (insert name)
  (forward-char)
  (end-of-line))

(defun ado-new-label (&optional name)
  "Make a new label-definition file either in the current or `lbl' directory.
The optional NAME argument gives the name of the file, without extension.

Used interactively, you'll get asked for the name of the label,
and where the file should be saved.

Bound to \\[ado-new-label]"
  (interactive)
  (unless name
	(setq name (read-from-minibuffer "What is the name of the label? ")))
  (switch-to-buffer
   (generate-new-buffer
    (generate-new-buffer-name (concat name ".lbl"))))
  (ado-insert-boilerplate "lbl.blp")
  (if (and ado-label-dir (y-or-n-p "Put in local `lbl' directory? "))
      (cd (directory-file-name ado-label-dir))
    (pwd))				;put in to avoid troubles later!
  (ado-mode)
  (goto-char (point-min))
  (re-search-forward "def ")
  (insert name)
  (forward-char 1))

(defun ado-write-file-as-buffer-name ()
  "Write a file as its buffer name.

Written as a utility, but currently not used anywhere in `ado-mode'"
  (interactive)
  (let (this-buffer)
    (setq this-buffer
		  (buffer-name))
    (if (string-match "*" this-buffer)
		(save-buffer)
	  (write-file (substring this-buffer 0 (string-match "<" this-buffer)) ado-confirm-overwrite-flag))))

(defalias 'ado-save-program #'save-buffer
  "`ado-save-program' is obsolete as a special function.
Use the proper combination of a before-save-hook and
\\[save-buffer] to save things nicely.")

(defun ado-before-save-file ()
  "The default `before-save-hook' for `ado-mode'.

This updates the timestamp using
\\[ado-update-timestamp] (if the ado-update-timestamp flag is set), then
tries to determine a good file name using \\[ado-find-extension] and
\\[ado-make-ado-name] (primarily in the case of ado-files and help files),
before running \\[save-buffer].

Timestamps are updated only if there is a '*! version xxx'
statement in ado files, a {* Last Updated: } or a {* <date>}{...}
in sthlp (or hlp) files."
  (interactive)
;;  (message "Calling all hooks!")
  (let ((filename (ado-make-ado-name)))
	(setq ado-extension (ado-find-extension))
	(if ado-update-timestamp-flag
		(ado-update-timestamp))
	;; if file name specified, this is just a write-file
	(if filename
		(unless (string= (concat default-directory filename) (buffer-file-name))
		  (set-visited-file-name filename)
		  (if (and ado-confirm-overwrite-flag
				   (file-exists-p (buffer-file-name)))
			  (unless (y-or-n-p (concat "Overwrite file " filename "? "))
				(error "Canceled")))))))
  
(defun ado-update-timestamp ()
  "Update the timestamp for the file, which is useful when saving.
 
Since Stata has no conventions about headers for files, `ado-mode' will:

Look for a '*! version xxx' statement in ado/do/ files, a {
* version xxx } or a {* <date>}{...} in sthlp (or hlp)
files, or a version x.y.z <date> in other files."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (cond
	 ((or (string= ado-extension "ado")
		  (string= ado-extension "class")
		  (string= ado-extension "do"))
	  (when (re-search-forward "^[*]![ \t]+version[ \t]+[0-9\.]*[ \t]*" (point-max) t)
		(delete-region (point) (point-at-eol))
		(insert (ado-nice-current-date))))
	 ((or (string= ado-extension "hlp")
		  (string= ado-extension "sthlp"))
	  (cond
	   ((search-forward "[*] Last Updated: " (point-max) t)
		(delete-region (point) (point-at-eol))
		(insert (ado-nice-current-date))
		(insert "}{...}"))
	   ((re-search-forward "^[ \t]*{[*]+[ \t]+[*]![ \t]+version[ \t]+[0-9\.]*[ \t]*" (point-max) t)
		(delete-region (point) (point-at-eol))
		(insert (ado-nice-current-date))
		(insert "}{...}"))
	   ((looking-at "{smcl}[ \t]*")
		(goto-char (match-end 0))
		(forward-char)
		(when (looking-at "{[*] *")
		  (goto-char (match-end 0))
		  (delete-region (point) (point-at-eol))
		  (insert (ado-nice-current-date))
		  (insert "}{...}")))
	   (t nil)))
	 (t
	  ;;; not in ado or help file
	  (when (re-search-forward "^\\([*]!\\)*[ \t]+[Vv][Ee][Rr][Ss][Ii][Oo][Nn][ \t]+[0-9\.]*[ \t]*" (point-max) t)
		(delete-region (point) (point-at-eol))
		(insert (ado-nice-current-date)))))))
	
    ;; looking for a version number, so that the date stamp can be updated
;; leaves the new date and time, even if the file is not saved... dunno what
;; to do to fix this, since I cannot find error trapping for errors which do
;; not use 'signal to identify the errors (write-file does not use a signal)

(defun ado-set-ado-extension ()
  "`ado-set-ado' is obsolete, but left for backward compatitbility.

Use \\[ado-find-extension] instead.
It returns a value instead of setting a variable."
  (interactive)
  (setq ado-extension (ado-find-extension)))

(defun ado-sho<w-extension ()
  "`ado-mode''s best guess at the correct file extension for the current buffer.

Just an interface to \\[ado-find-extension]."
  (interactive)
  (message "I think the extension is `%s'" (ado-find-extension)))

(defun ado-find-extension ()
  "Choose the file extension based on the file contents.
 
Since Stata has started getting more complicated, will fall back to
the current file extension if confused. Returns its best guess at the
extension. Not as reliable as it should be.

To test this, try \\[ado-show-extension]."
  ;; try to find the name, trust the buffer if lost, but issue warning if
  ;; lost or a contradiction
  (let (sez-file sez-contents)
	(setq sez-file
		  (if (and buffer-file-name (file-name-extension buffer-file-name))
			  (downcase (file-name-extension buffer-file-name))
			nil))
	(setq sez-contents
		  (save-excursion
			(goto-char (point-min))
			(ado-skip-header-lines) ;; skips special header lines
			(cond
			 ((looking-at "{smcl}")
			  (if (ado-find-help-name-start)
				  ado-help-extension
				"smcl"))
			 ((re-search-forward "^class[ \t]+" nil t) "class")
			 ((re-search-forward "^[ \t]*DIALOG" nil t) "dlg")
			 ;; the rest depend on where first occurances are
			 ((re-search-forward "^p\\(r\\|ro\\|rog\\|rogr\\|rogra\\|rogram\\)[ \t]+\\(d\\(e\\|ef\\|efi\\|efin\\|efine\\)[ \t]+\\)?" (point-at-eol) t)
					 "ado")
			 ((re-search-forward "^[ \t]*mata[ \t]*:[ \t]*$" (point-at-eol) t)
				   "mata")
			 (t "do"))
				;; (if first-program
				;; 	(if first-mata
				;; 		(if (< first-program first-mata) "ado" "mata")
				;; 	  "ado")
				;;   (if first-mata "mata" "do"))
			))
	;; rule the file extension as correct automatically
	(if sez-file
		(progn
		  (unless (string= sez-file sez-contents)
			(message "ado-mode thinks that the extension should be `%s' even though the current extension is `%s'!" sez-contents sez-file))
		  sez-file)
	  sez-contents)))

(defun ado-show-ado-name ()
  "`ado-mode''s best guess at the correct file name for the current buffer.

Just an interface to `ado-make-ado-name'."
  (interactive)
  (message "Suggested name: `%s'" (ado-make-ado-name)))

(defun ado-make-ado-name ()
  "Create a file name from the contents of the file.

Assumes that `ado-extension' has been set properly by \\[ado-find-extension].
Returns nil if the name of the file cannot be determined from the file contents.
You can look to see what name will be made via \\[ado-show-ado-name].

The command works differently depending on the type of file:

  .ado files: looks for the first 'program define' statement, and
    attaches the extension .ado. This means that there is no worry
    about the program name and file name being different. *Note* that
    the 'program define' statement may be abbreviated according to
    command rule, but must be at the start of a line. Also: it must be
    the first program defined in the .ado file.

  .class files: looks for the first 'class' statement, and attaches
    the extension .class. This ensures that the class name and the
    class filename match.

  .sthlp files: looks for first {hi:help ...} or {cmd:help ...}, and takes
    file name from the next chunk. This has problems with postestimation
    help files.

  all other Stata-related files: These do not need anything within the
    file to match the file name in order to run properly. So... if the
    file starts with *! in the first line, `ado-mode' will check
    lines starting with *!'s at the top of the program for something
    approaching a filename with the proper extension. If it is found,
    it will be used as the file name."
  (interactive)
  (let (name-start name-end)
    (save-excursion
      (goto-char (point-min))
	  (if (or (string= ado-extension "hlp")
			  (string= ado-extension "sthlp"))
		  (ado-make-help-name) ;; split out b/c of Stata 12
		(setq name-start
			  (cond ((string= ado-extension "class")
					 (re-search-forward "^class[ \t]+" nil t))
					((string= ado-extension "ado")
					 (re-search-forward "^pr\\(o\\|\\og\\|\\ogr\\|\\ogra\\|\\ogram\\)[ \t]+\\(de\\(f\\|fi\\|fin\\|fine\\)[ \t]+\\)?" nil t))
					((string= ado-extension "lbl")
					 (re-search-forward "^[ \t]*^la\\(b\\|be\\|bel\\)[ \t+]+de\\(f\\|fi\\|fin\\|fine\\)[ \t]+" nil t))
					(t nil)))
		(if (not name-start)
			(file-name-nondirectory (buffer-file-name))
		  ;;; !! need to split out things which can have spaces,
		  (re-search-forward (concat ado-stata-name-regexp "\\b"))
		  (setq name-end (point))
		  (concat (buffer-substring-no-properties name-start name-end) "." ado-extension))))))

(defun ado-make-help-name ()
  "Figure out the name of a help file from its contents.
  
Creates a file name from the contents of a help file, assuming that
the `ado-extension' has been set properly. Throws an error if the name cannot
be determined. This was split from \\[ado-make-file-name] because of big
changes to help files in Stata 12 (and the initial buggy fix)."
  (interactive)
  (let (full-name) ; titlepos syntaxpos (name-start nil))
	(save-excursion
	  (goto-char (point-min))
	  (cond
	   ((search-forward-regexp "{mansection[ \t]+.*?[ \t]+\\([^ :	]*\\).*}" nil t)
		(setq full-name (mapconcat #'identity (split-string (match-string-no-properties 1 nil)) "_")))
	   ((search-forward-regexp "{manlink[ \t]+.*?[ \t]+\\(.*?\\)[ \t]*}" nil t)
		(setq full-name (mapconcat #'identity (split-string (match-string-no-properties 1 nil)) "_")))
	   ((re-search-forward "{\\(bf\\|cmd\\|hi\\):[ \t]*help[ \t]+\\(.*?\\)[ \t]*}" nil t)
		(setq full-name (mapconcat #'identity (split-string (match-string-no-properties 2 nil)) "_")))
	   ((re-search-forward "{\\(bf\\|cmd\\|hi\\):[ \t]*\\(.*?\\)[ \t]*}" nil t)
		(setq full-name (mapconcat #'identity (split-string (match-string-no-properties 2 nil)) "_")))
	   ((search-forward "help for " nil t) ; very old help
		(re-search-forward "{\\(bf\\|cmd\\|hi\\):[ \t]*\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)\\b" nil t)
		(setq full-name (mapconcat #'identity (split-string (match-string-no-properties 1 nil)) "_")))
	   (t (error "Could not figure out help file name!"))))
	(concat full-name "." ado-extension)))
  
(defun ado-find-help-name-start ()
  "Return the point at which the command documented in a help file begins.

Need a function for this, because this location changed drastically between
Stata versions 11 and 12."
  (interactive)
	;; first find where Title and Syntax are, since these are bounds for searches
  (let ((debug-on-error t)) ; titlepos syntaxpos (name-start nil))
	(save-excursion
	  (goto-char (point-min))
	  (cond
	   ((search-forward-regexp "{manlink[ \t]+.*?[ \t]+" nil t)
		(point)) ;; have name-start set properly already from official Stata help
	   ((re-search-forward "{\\(bf\\|cmd\\|hi\\):[ \t]+" nil t)
		(point)) ;; name-start set properly by Stata 11 official help and some old help files
	   ((search-forward "help for " nil t) ; very old help
		(re-search-forward "{\\(bf\\|cmd\\|hi\\):" nil t))
	   (t nil)))))

(defun ado-find-help-name-start-pre12 ()
  "Return the point at which the command documented in a help file starts.

Guesses at where the help for a help file from Stata 11 or earlier.
This location changed drastically between Stata versions 11 and 12."
  (interactive)
	;; first find where Title and Syntax are, since these are bounds for searches
  (let ((debug-on-error t) titlepos syntaxpos (name-start nil))
	(save-excursion
	  (goto-char (point-min))
	  (setq titlepos (search-forward "Title" nil t))
	  (when titlepos
		;; search as far as "Title" to find {...:help !!!} for <= Stata 12 help
		(goto-char (point-min))
		(setq name-start
			  (re-search-forward "{\\(bf\\|cmd\\|hi\\):help[ \t]+" titlepos t))
		(unless name-start
		  ;; have stata 12-style help
		  (goto-char titlepos)
		  (setq syntaxpos (search-forward "Syntax" nil)) ;; want error if no Syntax
		  (goto-char titlepos)
		  (setq name-start
				(re-search-forward "{cmd:[ \t]*" syntaxpos t)))))
	name-start))

(defun ado-find-local-name ()
  "Return the name of the defining program in which the point is sitting.

If the program define statement cannot be found, returns nil. Returns a mess if
the program name is missing."
  (let (name-start name-end)
    (save-excursion
      (when (re-search-backward "^pr\\(o\\|\\og\\|\\ogr\\|\\ogra\\|\\ogram\\)[ \t]+" 0 t) ;goes to most recent definition
	    (setq name-start
			  (re-search-forward
			   "^pr\\(o\\|\\og\\|\\ogr\\|\\ogra\\|\\ogram\\)[ \t]+\\(de\\(f\\|fi\\|fin\\|fine\\)[ \t]+\\)*" nil t)
			  name-end (re-search-forward "[a-zA-Z_]+[a-zA-Z0-9_]*\\b"))
	    (buffer-substring-no-properties name-start name-end)))))

(defun ado-show-local-name ()
  "Show the value that would be returned by `ado-find-local-name'."
  (interactive)
  (message "The local program is `%s'" (ado-find-local-name)))

(defun ado-insert-file-and-indent (file)
  "Interactively insert (and properly indent) tbe file FILE.

Can also be called from within another program for inserting and indenting."
  (interactive "fGive the file to insert: ")
  (let ((beg (point)))
    (insert-file-contents file)
    (ado-indent-region beg (point))))

(defun ado-insert-boilerplate (file-name &optional raw full-path)
  "Generic command for inserting the boilerplate (template) file FILE-NAME.

The optional second argument RAW inserts the raw template without
  any substitutions.
The optional FULL-PATH argument specifies that FILE-NAME contains the full path
  for the file."
 
  (if full-path
	  (setq full-path file-name)
	(if (not ado-site-template-dir)
		(error "%s" "Use \\[set-variable] to set ado-site-template-dir to the directory holding the ado templates!"))
	(setq full-path (concat (file-name-as-directory ado-site-template-dir) file-name)))
  (if raw
      (insert-file-contents full-path)
    (ado-insert-file-and-indent full-path)))


;;; depth and indentation commands
(defun ado-line-starts-with-end-comment ()
  "Check if a line starts with an */.

Needed for old-school contintuation comments."
  (interactive)
  (save-excursion
	(beginning-of-line)
	(and (search-forward "*/" (point-at-eol) t)
		 (not (re-search-forward "[ /t]*$" (point-at-eol) t))
		 (not (search-backward "/*" (point-at-bol) t)))))

(defalias 'ado-out-of-nested-comment #'ado-start-of-nested-comment
  "Alias for `ado-start-of-nested-command'. Was the original name, but was,
quite frankly, a dumb name. Left as an alias for backward compatibility.")

(defun ado-start-of-nested-comment (&optional top from-level)
  "Move to start of nested comment, if necessary.

If inside a nested comment, move to its start. Otherwise, stay put.

If non-nil, the optional first argument TOP says to move to the top (outermost)
  level of nesting. If TOP is nil, this moves outside the current (possibly)
nested comment. The optional second-argument FROM-LEVEL is the nesting level
of the calling command."
  
  (interactive)
  (let ((ppsexp (parse-partial-sexp 1 (point))) this-level)
	(if (numberp (setq this-level (nth 4 ppsexp)))
		(if (or (not from-level) (<= from-level this-level))
			(if (search-backward "/*" 1 t)
				(if top (ado-start-of-nested-comment t)
				  (ado-start-of-nested-comment nil this-level))))
	  (if top
		  (if (search-backward "*/" (point-at-bol) t)
			  (ado-start-of-nested-comment top))))))

(defun ado-show-depth () "Show the depth of the command (for indenting).

An interactive interface to `ado-find-depth'"
  
  (interactive)
  (let ((depth (ado-find-depth)))
    (message "The depth is %d %s"
			 (car depth)
			 (if (nth 1 depth) " with continuation" " without continuation"))))

;; changed left-indenting of 'version' to left-indent iff spelled out to
;;   avoid the indenting, use an abbreviation
(defun ado-find-depth ()
  "Find the nesting depth of a command. Used for determining how far a command must be indented."
  (let (depth ppsexp in-continuation (oddend (ado-line-starts-with-end-comment)))
	(save-excursion
	  ;; going back to start of command in case of being in a comment
	  (setq in-continuation
			(ado-beginning-of-command))
	  ;; !! (setq ppsexp (parse-partial-sexp 1 (point)))
	  ;; !! (setq depth (car ppsexp))
	  (setq depth 0)
      ;; oddities which might need unindenting
      (when (or oddend
				(and (not ado-close-under-line-flag) (looking-at "}"))
				(looking-at "ver\\(s\\|si\\|sio\\|sion\\)"))
		(setq depth (1- depth)))
	  (end-of-line)
	  (setq depth (- depth (how-many "^[ \t]*\\(end$\\|end[ \t]+\\)" 1 (point))))
	  (beginning-of-line)
      ;; words which start blocks
	  ;; none of these are nestable; perhaps there should be a flag?
	  ;; need to be careful, because of program dir, drop, and list
      (setq depth (+ depth (how-many "^[ \t]*\\(input\\|\\(p\\(r\\|ro\\|rog\\|rogr\\|rogra\\|rogram\\)\\([ \t]+d\\(ef\\|efi\\|efin\\|efine\\)\\)?\\)\\)[ \t]+" 1 (point))))
	  (setq depth (+ depth (how-many "^[ \t]*\\(mata\\|python\\):?\\([ \t]+$\\|[ \t]+//\\( \\|$\\)\\|$\\)" 1 (point))))
	;; words which end blocks need to be deducted
	  ;;  plus overcounted 'program's
	  (setq depth (- depth (how-many "^[ \t]*p\\(r\\|ro\\|rog\\|rogr\\|rogra\\|rogram\\)[ \t]+\\(d\\(i\\|ir\\)\\|\\(\\(l\\|li\\|lis\\|list\\)\\|drop\\)\\)\\([ \t]+\\|$\\)" 1 (point)))))
	;; back at original point
	(save-excursion
	  (beginning-of-line)
	  (setq ppsexp (parse-partial-sexp 1 (point)))
	  (list (+ depth (car ppsexp)) in-continuation))))

(defun ado-indent-region (&optional start end)
  "Indent region.
Optional arguments START and END bound the region. If only START is specified,
then the region from START to point gets indented."

  (interactive)
  (let (endmark)
    (when (and (null start) (null end))
	  (setq start (min (point) (mark)))
	  (setq end (max (point) (mark))))
    (save-excursion
      (goto-char start)
      (beginning-of-line)
      (setq endmark (copy-marker end))
;;    (while (and (bolp) (not (eobp)) (< (point) endmark))
	  (while (< (point) endmark)
		;; doesn't clean up garbage whitespace lines (for speed in large buffers)
		(skip-chars-forward " \t\n")
		(ado-indent-line)
		(forward-line 1)))))

(defun ado-indent-buffer ()
  "Indent entire buffer."
  
  (interactive)
  ;; (message "ado-indent-buffer started: %s" (current-time-string))
  (save-excursion
    (ado-indent-region (point-min) (point-max)))
  ;; (message "ado-indent-buffer ended: %s" (current-time-string))
  )

(defun ado-indent-line ()
  "A smart indenter for ado files.
Many of the parameters can be customized using '\\[customize-group] ado'."

  (interactive)
  (if ado-smart-indent-flag
      (let (indent
			depth
			beg
			shift-amt
			(pos (- (point-max) (point))))
		(beginning-of-line)
		(setq beg (point))
		(cond ((and ado-delimit-indent-flag (looking-at "[ \t]*#d\\(e\\|el\\|eli\\|elim\\|elimi\\|elimit\\)?[ \t]+\\(;|cr\\)?"))	;#delimits belong at delimit indent; only if complete
			   (setq indent ado-delimit-indent-column))
			  ((and ado-comment-indent-flag
					(or (looking-at "^\\*") (looking-at "^*")))
			   (setq indent ado-comment-indent-column))
			  ((and ado-debugging-indent-flag
					(or (looking-at "^[ \t]*pause")
						(looking-at "^[ \t]*set t\\(r\\|ra\\|rac\\|race\\)[ \t]+")))
			   (setq indent ado-debugging-indent-column)) ; debugging at proper column (usually 0)
			  (t (setq indent (* tab-width (car (setq depth (ado-find-depth)))))   ; regular indentation
				 (if (nth 1 depth)
					 (setq indent (+ indent ado-continued-statement-indent-spaces)))))
		(skip-chars-forward " \t")
		(setq shift-amt (- indent (current-column)))
		(if (zerop shift-amt)
			(if (> (- (point-max) pos) (point))
				(goto-char (- (point-max) pos)))
		  (delete-region beg (point))
		  (indent-to indent)
		  ;; If initial point was within line's indentation,
		  ;; position after the indentation.  Else stay at same point in text.
		  (if (> (- (point-max) pos) (point))
			  (goto-char (- (point-max) pos))))
		shift-amt)))

(defun ado-clean-buffer ()
  "Turn all whitespace-only lines into empty lines while keeping blank lines."
  (interactive)
  ;; (message "Started ado-clean-buffer: %s" (current-time-string))
  (save-excursion
	(let (endmark)
	  (goto-char (point-min))
	  ;; Advance to first nonblank line.
	  (beginning-of-line)
	  (setq endmark (copy-marker (point-max)))
	;;    (while (and (bolp) (not (eobp)) (< (point) endmark))
	  (while (< (point) endmark)
	  (if (looking-at "^[ \t]*$")
		  (delete-region (point) (line-end-position)))
	  (forward-line 1))))
  (message "Ended ado-clean-buffer: %s" (current-time-string)))


(defun ado-delimit-is-semi-p ()
  "Return t if semicolons delimit commands, otherwise return nil."
  
  (save-excursion
    ;; if #delimit command is on same line, the delimiter is always cr
    (let ((line-start (point-at-bol)))
      (if (re-search-backward "#[ \t]*d\\(e\\|el\\|eli\\|elim\\elimi\\elimit\\)" 1 t)
		  (let ((ppsexp (parse-partial-sexp 1 (point))))
			(if (or
				 (nth 3 ppsexp)		; inside a string
				 (nth 4 ppsexp)		; inside a non-nestable comment
				 (nth 7 ppsexp))		; inside a type-b comment
				(ado-delimit-is-semi-p)
			  (if (>= (point) line-start)
				  nil
				(forward-sexp)
				(skip-chars-forward " \t")
				(looking-at ";"))))))))

(defun ado-show-delimiter ()
  "Show the value of the command delimiter at the insertion point."
  
  (interactive)
  (message "The delimiter is %s"
		   (if (ado-delimit-is-semi-p) ";" "cr")))

(defun ado-beginning-of-command ()
  "Move to the start of the command containg the insertion point is sitting.

This will jump back to the start of a command if the insertion point is within
the command, and jump forward to the start of a command if the delimiter is in
whitespace preceding a command.

When the delimiter is cr, blank lines count as empty commands, so the
cursor will stay put.

When the delimiter is ; even being just after a ; will mean that
the cursor is after the command, and will cause a jump forward. This is
technically correct, even if a bit disturbing.

This is known to have trouble when there are bizarre constructions, such
as /* */-style comments in the middle of a line, or something perverse like
   regress mpg // this is a comment /// with a fake continuation
     weight

Returns t if inside of a continued function, nil otherwise."
  (interactive)
  (let ((in-continuation nil)
		(start-line (line-number-at-pos))
		(skip-chars " \t"))
    (ado-out-of-nested-comment t)
    ;; first look backwards for either delimiter or delimit command
	(if (= start-line 1)
		(beginning-of-line)
	  (unless (search-backward "#delimit" (point-at-bol) t)
		(if (ado-delimit-is-semi-p)
			(progn
			  (setq skip-chars " \t\n")
			  (search-backward ";" 1 t)
			  (if (and
				   (< (line-number-at-pos) start-line)
				   (save-excursion
					 (skip-chars-forward " \t\n;")
					 (< (line-number-at-pos) start-line)))
				  (setq in-continuation t)))
		  (search-backward "\n" 1 t))
		(if (bobp)
			(forward-char 1)
		  (if (re-search-backward "\\(\\s-\\|^\\)///+.*$" (point-at-bol) t)
			  (progn
				(forward-char -1)
				(ado-beginning-of-command)
				(setq in-continuation t))
			(forward-char 1)))))
	  ;;    (skip-chars-forward " \t\n")
    (skip-chars-forward skip-chars)
    in-continuation))

(defun ado-end-of-command ()
  "Move to the start of the command containg the insertion point is sitting.

This can be fooled by internal /* */-style commands extending across lines."
  (interactive)
  (let (ppsexp)
    (if (ado-delimit-is-semi-p)
		(if (not (search-forward ";" nil t))
			(error "No end of command")
		  (setq ppsexp (parse-partial-sexp 1 (point)))
		  (if (or (nth 4 ppsexp) (nth 3 ppsexp) (nth 7 ppsexp))
			  (ado-end-of-command)
			(backward-char)))
      (if (re-search-forward "/\\*" (point-at-eol) t)
		  (progn
			(backward-char 2)
			(skip-syntax-backward "-"))
		;; look for continuation
		(if (or (search-forward "///" (point-at-eol) t)
				(search-backward "///" (point-at-bol) t))
			(progn
			  (end-of-line)
			  ;; will error out if narrowed after continuation
			  (if (eobp)
				  (error "No end of command - moved as far as possible")
				(forward-char)
				(ado-end-of-command)))
		  (if (not (search-forward "//" (point-at-eol) t))
			  (end-of-line)
			(backward-char 2)
			(skip-syntax-backward "-")))))))

(defun ado-copy-command (&optional to-kill-ring-flag)
  "Copy the command containing the insertion point to the clipboard.
If optional first argument `TO-KILL-RING-FLAG` is nil, the command
is copied to the  kill ring, otherwise it is copied to the clipboard.

May be called interactively, but meant for using within functions
working on regions."
  (interactive)
  (let ((start-here
		 (save-excursion
		   (ado-beginning-of-command)
		   (point)))
		(end-here
		 (save-excursion
		   (ado-end-of-command)
		   (point)))
		 (select-enable-clipboard t))
	(if to-kill-ring-flag
		(buffer-substring-no-properties start-here end-here)
	  (kill-ring-save start-here end-here))))

;; stolen from c-mode, and changed slightly, since Stata does not use
;; braces on separate lines
(defun ado-electric-closing-brace (arg)
  "Insert closing character and correct line's indentation.
ARG is the character being inserted.
Character could be inserted on new line."
  (interactive "P")
  (if (and (not arg)
		   (eolp)
		   ado-closing-brace-alone-flag)
      (if (save-excursion
			(skip-chars-backward " \t")
			(not (bolp)))
		  (newline-and-indent)))
  (ado-electric-brace arg))

(defun ado-electric-brace (arg)
  "Insert character and correct line's indentation.
ARG is the character being inserted."
  (interactive "P")
  (let (insertpos)
    (when (and (not arg)
			   (eolp)
			   ado-auto-newline-flag)
	  (insert last-command-event)
	  (ado-indent-line)
	  (when ado-auto-newline-flag
		(newline)
		;; (newline) may have done auto-fill
		(setq insertpos (- (point) 2))
		(ado-indent-line))
	  (save-excursion
		(if insertpos (goto-char (1+ insertpos)))
		(delete-char -1)))
    (if insertpos
		(save-excursion
		  (goto-char insertpos)
		  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun ado-newline ()
  "Justify current line before doing a `newline-and-indent'."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (ado-indent-line))
  (newline-and-indent))

(defun ado-split-line ()
  "Split line at point, putting in the proper continuation characters.

What gets inserted depends on the value of `ado-use-modern-split-flag'"
  (interactive)
  (if ado-line-up-continuations
		(progn
		  (newline)
		  (backward-char)
		  (ado-continuation-indent)
		  (forward-char)
		  (ado-indent-line)
		  (unless ado-use-modern-split-flag
			 (insert "*/")))
	(if ado-use-modern-split-flag
		(progn
		  (insert " ///")
		  (newline-and-indent))
	  (insert "/*")
	  (newline-and-indent)
	  (insert "*/"))))

(defun ado-macify-selection-or-word (&optional stringify)
  "Put `' or compound double quotes around current word or selction.
If optional argument STRINGIFY is non-nil, put compound double quotes around `'.

If point is not in a word and there is no selection insert `'.

The point is put after the macro.

Useful on non-US keyboards, where backticks can be painful to insert."

  (interactive)
  (let (noregion popmark)
	(unless (and transient-mark-mode mark-active)
	  (if (condition-case nil
			  (beginning-of-thing 'word)
			(error nil))
		  (progn
			(setq popmark 't)
			(mark-word))
		(setq noregion 't)))
	(if noregion
		(progn
		  (insert "`'")
		  (forward-char -1))
	  (save-excursion
		(goto-char (region-beginning))
		(if stringify (insert "`\""))
		(insert "`"))
	  (goto-char (region-end))
	  (insert "'")
	  (if stringify (insert "\"'"))
	  (if popmark (pop-mark)))))

(defun ado-strmacify-selection-or-word ()
  "Put current selection inside `' and then add compound double quotes."
  (interactive)
  (ado-macify-selection-or-word t))

(defun ado-stringify-selection ()
  "Put compound double quotes around the selection.

If nothing is selected, inserts a pair of compound double quotes."
  (interactive)
  (if (and transient-mark-mode mark-active)
	  (progn
		(save-excursion (goto-char (region-beginning)) (insert "`\""))
		(goto-char (region-end)) (insert "\"'"))
	(insert "`\"\"'")
	(forward-char -2)))

(defun ado-electric-semi (repeat)
  "Put in extra newline if the semi-colon is the end-of-line delimiter.
REPEAT gives the number of newlines to insert."
  (interactive "P")
  (if (ado-delimit-is-semi-p)
      (ado-electric-brace repeat)
    (self-insert-command (prefix-numeric-value repeat))))


;;; 1. need some way to highlight ^[ /t]*- lines with one background?
;;; 2. need some way to highlight ^[ /t]*= lines with another background?
;;; 3. output in plain black text
;;; skipped steps in gray text
;;; could have three modes and change the faces locally?
;;; think about method for foreground by one, background by another

;; (defun ado-set-debug-highlight ()
;;   (interactive)
;;   (setq
;;    ado-font-lock-keywords
;;    ()))


;;; Working with help files
;;; allowing for the user's name to be put into help files
(defun ado-set-ado-signature-file ()
  "Query location of signature file."
  (interactive)
  (setq ado-signature-file
		(read-file-name "Set ado signature file to: "
						(file-name-directory
						 (expand-file-name
						  (if (not ado-signature-file)
							  (if (file-exists-p "~/.ado-signature")
								  "~/.ado-signature")
							ado-signature-file))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stuff for writing help files ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ado-new-help (&optional name)
  "Make a new help file either in the current or `new' directory.

Used interactively, you'll get asked for the name of the file and its purpose,
and where the file should be saved.

Used programatically, the optional NAME option can be used for the name of the
help file.

To get the full benefit of a signature, you need a variable which contains the
name of the signature file or a .signature file.

Bound to \\[ado-new-help]"
  (interactive)
  (unless name
    (setq name (read-from-minibuffer "For what program should help be written? ")))
  (switch-to-buffer
   (generate-new-buffer
	(generate-new-buffer-name (concat name "." ado-help-extension))))
  (ado-insert-boilerplate "help.blp" t)
  (if (and ado-new-dir (y-or-n-p "Put in 'new' directory? "))
	  (cd (directory-file-name ado-new-dir)))
  (goto-char (point-min))
  (while (search-forward "XXX" nil t)
	(replace-match name t))
  (goto-char (point-min))
  (search-forward "version #.#.# ")
  (delete-region (point) (point-at-eol))
  (insert (ado-nice-current-date))
  (insert "}{...}")
  ;; new logic for authorship:
  ;;   check prompt flag, if turned off, do NOT use ANY signature
  ;;   if flag on, but there is no file, use user name only
  (search-forward "{title:Author}")
  (if ado-help-author-flag
	  (progn
		(search-forward "{pstd}")
		(forward-line)
		(delete-region (point) (point-at-eol))
		(if (or ado-signature-file
				(if ado-signature-prompt-flag
					(ado-set-ado-signature-file)))
			(insert-file-contents ado-signature-file)
		  ;; no signature file (complicated because of past defaults)
		  (if (or (not ado-claim-name) (string= ado-claim-name ""))
			  (setq ado-claim-name
					(read-from-minibuffer "Whose name(s) should be used as authors? " user-full-name)))
		  (insert ado-claim-name)))
	;; do not want authorship
	(let ((end-here
		   (save-excursion
			 (search-forward "{title:References}")
			 (beginning-of-line)
			 (point))))
	  (beginning-of-line)
	  (delete-region (point) end-here)))
  (ado-mode)
  ;; turn off indenting
  (setq ado-smart-indent-flag nil)
  (ado-save-program)
  (goto-char (point-min))
  (search-forward "title of command")
  (beginning-of-line))

(defun ado-toggle-help-extension ()
  "Toggle the help file extension between sthlp and hlp.

Useful for those writing help files meant to be used in Stata 7 through Stata 9,
namely those maintaining old ado-files."
  (interactive)
  (if (string= ado-help-extension "sthlp")
	  (setq ado-help-extension "hlp")
	(setq ado-help-extension "sthlp"))
  (message "ado-help-extension is now `%s'" ado-help-extension))

;;; useful insertions in smcl
(defun ado-help-insert-option-in-body (&optional option-name)
  "Insert an option in a help file.
The optional argument OPTION-NAME is, well duh, the option name."
  (interactive)
  (if (not option-name)
      (setq option-name
			(read-from-minibuffer "What is the full name of the option? " option-name)))
  (ado-insert-with-lfd (concat "{p 0 4}{cmd:" option-name "}"))
  (ado-insert-with-lfd "{p_end}")
  (newline)
  (forward-line -2)
  (end-of-line)
  (forward-char -1))

(defun ado-new-cscript (&optional desc name)
  "Start a new certification script.
The optional first argument DESC is for the description of the certification
  script.
The optional secon argument NAME is for the name of the script.

Used interactively, you'll get asked for the name of the commands to be checked
as well as an optional descriptions.

Unlike the other functions which create new Stata files, this just opens a
buffer and puts in the proper header for a cert script. No file gets saved.

Bound to \\[ado-new-cscript]."
  (interactive)
  (unless name
    (setq name (read-from-minibuffer "For what ado-file(s) should a cert script be written? ")))
  (unless desc
	(setq desc (read-from-minibuffer "What description would you like? (blank to match ado-file(s)) ")))
  (if (string= desc "")
	(setq desc name))
  (switch-to-buffer
   (generate-new-buffer
	(generate-new-buffer-name (concat name ".do"))))
  (insert "cscript \"" desc "\" adofile " name)
  (ado-mode))

(defun ado-insert-with-lfd (sometext)
  "Insert and indent SOMETEXT."
  (insert sometext)
  (newline-and-indent))

(defun ado-nice-current-date ()
  "Return the current date and time as specified `ado-date-format'.

See `format-time-string' for help on setting your favorite date format."
  (concat
   (if ado-lowercase-date-flag
       (downcase (format-time-string ado-date-format))
     (format-time-string ado-date-format))
   (if ado-initials-flag (concat " " ado-initials))))

(defun ado-insert-nice-current-date ()
  "Insert a nice current date.

Wee \\[ado-nice-current-date] for details"
  (interactive)
  (insert (ado-nice-current-date)))

(defun ado-set-imenu-items ()
  "Function for setting imenu items.

Not really that worthwhile for Stata because there are typically not that
many program defines in an ado file. Still."
  (interactive)
  (setq imenu-case-fold-search nil)
  (setq imenu-generic-expression
		(list
		 (list nil "^\\s-*pr\\(o\\|og\\|ogr\\|ogra\\|ogram\\)\\(\\s-+\\(de\\|def\\|defi\\|defin\\|define\\)?\\)\\s-+\\([a-zA-Z_][a-zA-Z_0-9]*\\)" 4))))

;; for finding lists of directories where Stata has files
(defun ado-find-ado-dirs (dir &optional subdir)
  "Find directories where Stata stores files.
DIR is the directory to look in.
The optional second argument SUBDIR gives the subdirectories to look in.
Allowable values are
   all - look in the directory and all single-letter-or-digit subdirectories
   sub - look just in the single-letter-or-digit subdirectories
  self - look just in the given directory
The strange single-letter-or-digit subdirectories come from Stata storing
both its own and downloaded files in such directories. A remnant of old file
systems with 255-file limits."
  
  (interactive)
  (unless subdir
	(setq subdir "all"))
  (append
   (if (or (string= subdir "all") (string= subdir "self"))
	   (list dir))
   (if (or (string= subdir "all") (string= subdir "sub"))
	   (directory-files dir t "^[a-z_0-9]$"))
   nil))

(defun ado-next-error (&optional goback)
  "Look for next error in a log file (smcl or txt).

If the optional argument GOBACK is non-nil, looks backwards (see also
\\[ado-prev-error]).

If looking through a file where tracing is on, goes back to the line which caused the error."
  (interactive)
  (let (whereto traceoff traceon)
	(save-excursion
	  (if goback
		  (setq whereto (re-search-backward "^\\({err}\\|^r([1-9][0-9]*)\\)" nil t))
		(setq whereto (re-search-forward "^\\({err}\\|^r([1-9][0-9]*)\\)" nil t))))
	(if whereto
		(progn
		  (goto-char whereto)
		  (beginning-of-line)
		  (save-excursion
			(setq whereto (re-search-backward "^[ \t]*\\(-\\|=\\)" nil t)))
		  (when whereto
				(save-excursion
				  (setq traceoff (search-backward "set trace off" nil t)))
				(if traceoff
					(progn
					  (save-excursion
						(setq traceon (search-backward "set trace on" nil t)))
					  (if (< traceon traceoff)
						  (if (< traceoff whereto)
							  (goto-char whereto))))
				  (goto-char whereto))))
	  (message "No %s error found"
			   (if goback "previous" "next")))))

(defun ado-prev-error ()
  "Look for previous error in a log file (smcl or txt).

See `ado-next-error' for more details."

  (interactive)
  (ado-next-error t))

;; matching parens
(defun ado-balance-brace (&optional block)
  "Select contents inside balanced braces but outside strings or comments.

By default, braces are any of {[()]}.

If BLOCK is non-nil, include enclosing loop-inducing commands
such as -foreach-. If there are no curly braces to match, behave
as though BLOCK were nil.

Note: this uses the sloppy matching which comes with Emacs,
so things like (] match. This is not good, but it
has the (minor) advantage of allowing the syntax tables to define what
a brace should be."
 
  (interactive)
  (let (here there (ppsexp (parse-partial-sexp 1 (point))))
	(save-excursion
	  ;; jump to start of comment/string so scan-lists works
	  (if (or (nth 3 ppsexp) (nth 4 ppsexp))
		  (goto-char (nth 8 ppsexp)))
	  ;; if no block, these error out
	  (setq here
			(condition-case nil
				(scan-lists (point) -1 1)
			  (error)))
	  (setq there
			(condition-case nil
				(scan-lists (point) 1 1)
			  (error))))
	(if (or (not here) (not there))
		(if (not here)
			(if (not there)
				(message "Not inside braces")
			  (message "No starting brace found"))
		  (message "No ending brace found"))
	  ;; Jump to start of loop if needed
	  (goto-char here)
	  (set-mark there)
	  (when block
		(if (looking-at "[[(]")
			(setq there (ado-balance-brace t))
		  (ado-beginning-of-command)
		  (unless (looking-at "[\t ]*\\(\\(for\\(each\\|\\(v\\|va\\|val\\|valu\\|value\\|values\\)\\)\\)\\|while\\)[\t ]+")
			(goto-char here)))))))

(defun ado-grab-block ()
  "Select a code block in a smart fashion, knowing about ifs and loops."
  (interactive)
  (ado-balance-brace t))

(defun ado-send-block-to-stata ()
  "Grab code block and send to Stata."
  (interactive)
  (ado-grab-block)
  (ado-send-command-to-stata))

(defun ado-skip-special-comments ()
  "Move forward over all *! comments and empty lines from the current line.

Used to find the beginning of programs."
  (interactive)
  (goto-char (point-at-bol))
  (while (re-search-forward "^\\([*]!\\|[ \t]*$\\)" (point-at-eol) t)
	(forward-line)
	(goto-char (point-at-bol))))

(defun ado-skip-header-lines ()
  "Move forward over all header lines.

Skips * and *! comments, empty lines, and capture program drop lines
from the current line until they run out. Used to find the beginning of
programs, even those defined in a funky way."
  (interactive)
  (goto-char (point-at-bol))
  (while (and (not (eobp))
			  (goto-char (point-at-bol))
			  (re-search-forward "^[ \t]*\\([*]\\|$\\|vers\\|versi\\|version\\|\\(ca\\(p\\|pt\\|ptu\\|ptur\\|pture\\)[ \t]+p\\(r\\|ro\\|rog\\|rogr\\|rogra\\|rogram\\)[ \t]+drop\\)\\)" (point-at-eol) t))
	(forward-line)))

(defun ado-set-window-width (n)
  "Set the selected window's width.
As one might guess, N is the new width."
  (interactive "NWindow width:")
  (set-frame-width (selected-frame) n))

(defun ado-statacorp-defaults ()
  "Set editing options to StataCorp defaults.

This changes timestamps, where closing braces go, tab widths and other such
formatting details to match the internal StataCorp rules."
  (interactive)
  (set-variable 'ado-close-under-line-flag nil)
  (set-variable 'ado-lowercase-date-flag t)
  (set-variable 'ado-date-format "%d%b%Y")
  (set-variable 'ado-initials-flag nil)
  (set-variable 'tab-width 8)
  (ado-set-window-width 80)
  (message "%s" "ado-mode options set to StataCorp defaults"))

;; these trim functions come from Magnar Sveen via Xah Lee at
;; http://ergoemacs.org/emacs/modernization_elisp_lib_problem.html
;; http is intentional
(defun ado-string-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun ado-string-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun ado-string-trim (s)
  "Remove whitespace at the beginning and end of S."
  (ado-string-trim-left (ado-string-trim-right s)))
			 
;; Aquamacs emacs specifics (perhaps should be broken out?)
(if (boundp 'aquamacsxb-version)
    (define-key ado-mode-map [remap mac-key-save-file] 'ado-save-program))

(provide 'ado-mode)

;;; ado-mode.el ends here
