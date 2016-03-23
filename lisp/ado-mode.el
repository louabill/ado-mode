;;; ado-mode.el --- ado mode, and its idiosyncratic commands.

;; Copyright (C) 1996,..., 2016 Bill Rising

;; Maintainer: Bill Rising, brising at stata dot com
;; Keywords: ado-mode, highlighting
;; Version: 1.14.1.0 of March 23, 2016
;;
;; This file is NOT part of GNU Emacs.

;; This ado-mode is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This ado-mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this ado-mode; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:

;; This package provides the ado mode *NOT* documented in the
;; Emacs user's manual.
;; Long ago, this was based on the c-mode, but the similarities are
;;   likely now quite small
;;
;;; Code:

;;; required files
(require 'font-lock)
(require 'ado-cus)
(require 'ado-font)
(require 'ado-clip)
(require 'ado-to-stata)
(require 'ado-font-lock) ;; all the font-lock definitions
(require 'ado-stata-info) ;; gathering info from Stata

;;; putting in the proper extensions for using the ado-mode
(if (assoc "\\.ado$" auto-mode-alist) nil
  (setq auto-mode-alist 
		(append (list '("\\.ado\\'" . ado-mode)
					  '("\\.lbl\\'" . ado-mode)
					  '("\\.do\\'"  . ado-mode)	;could be a problem
					  '("\\.hlp\\'" . ado-mode) ; will be a problem for those programming in MS WinWhatever
					  '("\\.ihlp\\'" . ado-mode)
					  '("\\.sthlp\\'" . ado-mode) ; Stata's fix for pernicious .hlp files blockage
					  '("\\.dlg\\'" . ado-mode)
					  '("\\.smcl\\'" . ado-mode)
					  '("\\.class\\'" . ado-mode)
					  '("\\.mata\\'" . ado-mode)
					  '("\\.ADO\\'" . ado-mode) ; for MS-DOG files
					  '("\\.LBL\\'" . ado-mode)
					  '("\\.DLG\\'" . ado-mode)
					  '("\\.DO\\'"  . ado-mode)
					  '("\\.HLP\\'" . ado-mode)
					  '("\\.IHLP\\'" . ado-mode)
					  '("\\.SMCL\\'" . ado-mode) 
					  '("\\.STHLP\\'" . ado-mode)
					  '("\\.CLASS\\'" . ado-mode)
					  '("\\.MATA\\'" . ado-mode)
					  )
				auto-mode-alist
			   )))

(defvar ado-font-lock-keywords nil)
(defvar ado-font-lock-syntactic-keywords nil)
(defvar ado-extension nil)
(defvar ado-added-names nil)

;; abbrev table
(defvar ado-mode-abbrev-table nil
  "Abbrev table used while in ado mode.")
(define-abbrev-table 'ado-mode-abbrev-table ())

;; syntax table
(defvar ado-mode-syntax-table nil
  "Syntax table used while in ado mode.")

(if ado-mode-syntax-table
    ()
  (setq ado-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "." ado-mode-syntax-table) ;nullify escape meaning
  (modify-syntax-entry ?\$ "." ado-mode-syntax-table)
;; commented out, because ' can be used too many places, now.
;  (modify-syntax-entry ?` "(\'" ado-mode-syntax-table)
;  (modify-syntax-entry ?\' ")`" ado-mode-syntax-table)
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
(defvar ado-mode-map (make-sparse-keymap)
  "Keymap for Ado mode." )
(define-key ado-mode-map "\t"       'ado-indent-line)
(define-key ado-mode-map [(meta return)] 'ado-send-command-to-stata)
(define-key ado-mode-map [(meta control return)] 'ado-send-buffer-to-stata)
(define-key ado-mode-map [(meta shift return)] 'ado-split-line)
(define-key ado-mode-map "\C-c\C-a"   'ado-mode)
(define-key ado-mode-map "\C-c\C-b"   'ado-grab-block)
(define-key ado-mode-map "\C-c\M-b"   'ado-send-block-to-stata)
(define-key ado-mode-map "\C-c\C-c"   'ado-help-command)
(define-key ado-mode-map "\C-c\C-d"   'ado-new-help)
(define-key ado-mode-map "\C-c\C-e"   'ado-foreach-loop)
(define-key ado-mode-map "\C-c\C-f"   'ado-open-any-file)
(define-key ado-mode-map "\C-c\M-f"   'ado-open-command)
(define-key ado-mode-map "\C-c\C-h"   'ado-help-at-point)
(define-key ado-mode-map "\C-c\C-i"   'ado-insert-new-program)
(define-key ado-mode-map "\C-c\C-j"   'ado-strmacify-selection-or-word)
(define-key ado-mode-map "\C-c\C-l"   'ado-new-label)
(define-key ado-mode-map "\C-c\C-m"   'ado-macify-selection-or-word)
(define-key ado-mode-map "\C-c\C-n"   'ado-new-program)
(define-key ado-mode-map "\C-c\C-o"   'ado-open-any-file)
(define-key ado-mode-map "\C-c\M-o"   'ado-open-command)
(define-key ado-mode-map "\C-c\C-s"   'ado-stringify-selection)
(define-key ado-mode-map "\C-c\C-t"   'ado-input-to-stata)
(define-key ado-mode-map "\C-c\C-v"   'ado-forvalues-loop)
(define-key ado-mode-map "\M-a"       'ado-beginning-of-command)
(define-key ado-mode-map "\M-e"       'ado-end-of-command)
;;(define-key ado-mode-map "\C-c;"    'comment-region)
;;(define-key ado-mode-map "\C-c:"    'uncomment-region)
;; finally not needed anymore!
;;(define-key ado-mode-map "\C-x\C-s"   'ado-save-program)
(define-key ado-mode-map "{"          'electric-ado-brace)
(define-key ado-mode-map "}"          'electric-ado-closing-brace)
(define-key ado-mode-map ";"          'electric-ado-semi)

;;; menu bar definitions
(define-key ado-mode-map [menu-bar] (make-sparse-keymap))
(define-key ado-mode-map [menu-bar ado]
  (cons "Ado-mode" (make-sparse-keymap "Ado-mode")))
(define-key ado-mode-map [menu-bar ado options]
  (cons "Options" (make-sparse-keymap "options")))
(define-key ado-mode-map [menu-bar ado l3]
  '(menu-item "--single-line"))
(define-key ado-mode-map [menu-bar ado indent-buffer]
  '("Indent Region" . ado-indent-region))
(define-key ado-mode-map [menu-bar ado indent-buffer]
  '("Indent Buffer" . ado-indent-buffer))
(define-key ado-mode-map [menu-bar ado new]
  (cons "New program" (make-sparse-keymap "new")))
(define-key ado-mode-map [menu-bar ado l2]
  '(menu-item "--single-line"))
;;(define-key ado-mode-map [menu-bar ado uncomment-region]
;;  '("Uncomment Region" . uncomment-region))
;;(define-key ado-mode-map [menu-bar ado comment-region]
;;  '("Comment Out Region" . comment-region))
(define-key ado-mode-map [menu-bar ado ado-forvalues-loop]
  '("Forvalues loop" . ado-forvalues-loop))
(define-key ado-mode-map [menu-bar ado ado-foreach-loop]
  '("Foreach loop" . ado-foreach-loop))
(define-key ado-mode-map [menu-bar ado l1]
  '(menu-item "--single-line"))

;; place for customizations
(define-key ado-mode-map [menu-bar ado strmacify-word]
  '("Stringify and Macify Word or Selection" . ado-strmacify-selection-or-word))
(define-key ado-mode-map [menu-bar ado stringify-selection]
  '("Stringify Selection" . ado-stringify-selection))
(define-key ado-mode-map [menu-bar ado macify-word]
  '("Macify Word or Selection" . ado-macify-selection-or-word))
(define-key ado-mode-map [menu-bar ado l0]
  '(menu-item "--single-line"))
(define-key ado-mode-map [menu-bar ado ado-end-of-command]
  '("Go to end of command" . ado-end-of-command))
(define-key ado-mode-map [menu-bar ado ado-beginning-of-command]
  '("Go to beginning of command" . ado-beginning-of-command))
(define-key ado-mode-map [menu-bar ado l_1]
  '(menu-item "--single-line"))
(define-key ado-mode-map [menu-bar files save-buffer]
  '("Save buffer" . ado-save-program))
(define-key ado-mode-map [menu-bar ado save-program]
  '("Save buffer" . ado-save-program))



;; submenu New
(define-key ado-mode-map [menu-bar ado new ado-new-label]
  '("Label file" . ado-new-label))
(define-key ado-mode-map [menu-bar ado new ado-new-do]
  '("Do-file" . ado-new-do))
(define-key ado-mode-map [menu-bar ado new ado-new-help]
  '("Help file" . ado-new-help))
(define-key ado-mode-map [menu-bar ado new ado-new-cscript]
  '("Cert script" . ado-new-cscript))
(define-key ado-mode-map [menu-bar ado new ado-new-testado]
  '("New do-file for program testing" . ado-new-testado))
(define-key ado-mode-map [menu-bar ado new ado-insert-new-program]
  '("Insert new subprogram" . ado-insert-new-program))
(define-key ado-mode-map [menu-bar ado new ado-new-program]
  '("Generic new program" . ado-new-program))

;; submenu Options
;;; this submenu follows
(define-key ado-mode-map [menu-bar ado options special-indentation]
  (cons "Special Indentation" (make-sparse-keymap "special-indentation")))

(define-key ado-mode-map [menu-bar ado options ado-help-author-toggle]
  '(menu-item "Include Author section in help files"
			  (lambda () (interactive) (ado-toggle-flag 'ado-help-author-flag))
			  :button (:toggle . ado-help-author-flag)))

(define-key ado-mode-map [menu-bar ado options ado-send-to-all-toggle]
  '(menu-item "Send code to all matching Statas"
			  (lambda () (interactive) (ado-toggle-flag 'ado-send-to-all-flag))
			  :button (:toggle . ado-send-to-all-flag)))

(define-key ado-mode-map [menu-bar ado options ado-strict-match-toggle]
  '(menu-item "Use strict matching for finding Stata instance"
			  (lambda () (interactive) (ado-toggle-flag 'ado-strict-match-flag))
			  :button (:toggle . ado-strict-match-flag)))

(define-key ado-mode-map [menu-bar ado options ado-comeback-toggle]
  '(menu-item "Return to Emacs after submission"
			  (lambda () (interactive) (ado-toggle-flag 'ado-comeback-flag))
			  :button (:toggle . ado-comeback-flag)))

(define-key ado-mode-map [menu-bar ado options ado-open-read-only-toggle]
  '(menu-item "Open files from adopath in read-only mode"
			  (lambda () (interactive) (ado-toggle-flag 'ado-open-read-only-flag))
			  :button (:toggle . ado-open-read-only-flag)))

(define-key ado-mode-map [menu-bar ado options ado-confirm-overwrite-toggle]
  '(menu-item "Confirm File Overwrite"
	      (lambda () (interactive) (ado-toggle-flag 'ado-confirm-overwrite-flag))
	      :button (:toggle . ado-confirm-overwrite-flag)))

(define-key ado-mode-map [menu-bar ado options ado-comment-column-change]
  '(menu-item "Set Comment Column..." 
	      (lambda () (interactive) ado-change-number 'ado-comment-column 'ask)))

(define-key ado-mode-map [menu-bar ado options ado-continued-statement-indent-spaces-change]
  '(menu-item "Set Continuation Indentation..." ado-continued-statement-indent-spaces-change))

(define-key ado-mode-map [menu-bar ado options ado-tab-width-change]
  '(menu-item "Set Tab Width..." ado-tab-width-change))

(define-key ado-mode-map [menu-bar ado options ado-update-timestamp-toggle]
  '(menu-item "Update Timestamps on Save" 
	      (lambda () (interactive) (ado-toggle-flag 'ado-update-timestamp-flag))
	      :button (:toggle . ado-update-timestamp-flag)))

(define-key ado-mode-map [menu-bar ado options ado-fontify-new-toggle]
  '(menu-item "Fontify New Ado Files" 
	      (lambda () (interactive) (ado-toggle-flag 'ado-fontify-new-flag))
	      :button (:toggle . ado-fontify-new-flag)))

(define-key ado-mode-map [menu-bar ado options ado-auto-newline-toggle]
  '(menu-item "Automatic New Line" 
	      (lambda () (interactive) (ado-toggle-flag 'ado-auto-newline-flag))
	      :button (:toggle . ado-auto-newline-flag)))

(define-key ado-mode-map [menu-bar ado options ado-closing-brace-alone-toggle]
  '(menu-item "Closing Brace Alone" 
	      (lambda () (interactive) (ado-toggle-flag 'ado-closing-brace-alone-flag))
	      :button (:toggle . ado-closing-brace-alone-flag)))

(define-key ado-mode-map [menu-bar ado options ado-close-under-line-toggle]
  '(menu-item "Close Under Line"
	      (lambda () (interactive) (ado-toggle-flag 'ado-close-under-line-flag))
	      :button (:toggle . ado-close-under-line-flag)))

(define-key ado-mode-map [menu-bar ado options ado-use-modern-split-toggle]
  '(menu-item "Use Modern Line-split"
	      (lambda () (interactive) (ado-toggle-flag 'ado-use-modern-split-flag))
	      :button (:toggle . ado-use-modern-split-flag)))

(define-key ado-mode-map [menu-bar ado options ado-do-indent-toggle]
  '(menu-item "Indent Do Files"
	      (lambda () (interactive) (ado-toggle-flag 'ado-do-indent-flag))
	      :button (:toggle . ado-do-indent-flag)))

;; needs its own toggling function, because keymaps must be changed.
(define-key ado-mode-map [menu-bar ado options ado-return-also-indents-toggle]
  '(menu-item "Return also Indents" ado-return-toggle
	      :button (:toggle . ado-return-also-indents-flag)))

(define-key ado-mode-map [menu-bar ado options ado-smart-indent-toggle]
  '(menu-item "Smart Indent"
	      (lambda () (interactive) (ado-toggle-flag 'ado-smart-indent-flag))
	      :button (:toggle . ado-smart-indent-flag)
;	      :help "This is some help, ain't it?"
	      ))

;(define-key ado-mode-map [menu-bar ado options div1]
;  '(menu-item "--shadow-etched-in"))
;(define-key ado-mode-map [menu-bar ado options title]
;  '(menu-item "Check or Uncheck"))
;  '("Toggle smart indent" . ado-smart-indent-toggle))

;; subsubmenu Options/Special Indent
(define-key ado-mode-map [menu-bar ado options special-indentation ado-change-comment-indent]
  '(menu-item "Change comment indent column..." 
	      (lambda () (interactive) (ado-change-number 'ado-comment-indent-column 'ask))
	      :enable ado-delimit-indent-flag))

(define-key ado-mode-map [menu-bar ado options special-indentation ado-comment-indent-flag-toggle]
  '(menu-item "Comment column indentation" 
	      (lambda () (interactive) (ado-toggle-flag 'ado-comment-indent-flag))
 	      :button (:toggle . ado-comment-indent-flag)))

(define-key ado-mode-map [menu-bar ado options special-indentation ado-change-delimit-indent]
  '(menu-item "Change #delimit column..." 
	      (lambda () (interactive) (ado-change-number 'ado-delimit-indent-column 'ask))
	      :enable ado-delimit-indent-flag))

(define-key ado-mode-map [menu-bar ado options special-indentation ado-delimit-indent-flag-toggle]
  '(menu-item "#delimit indented differently" 
	      (lambda () (interactive) (ado-toggle-flag 'ado-delimit-indent-flag))
 	      :button (:toggle . ado-delimit-indent-flag)))

(define-key ado-mode-map [menu-bar ado options special-indentation ado-change-debugging-indent]
  '(menu-item "Change debugging column..." 
	      (lambda () (interactive) (ado-change-number 'ado-debugging-indent-column 'ask))
	      :enable ado-debugging-indent-flag))

(define-key ado-mode-map [menu-bar ado options special-indentation ado-debugging-indent-flag-toggle]
  '(menu-item "debugging indented differently" 
	      (lambda () (interactive) (ado-toggle-flag 'ado-debugging-indent-flag))
 	      :button (:toggle . ado-debugging-indent-flag)))

;; initial mode defintion function
(defun ado-mode ()
  "Major mode for editing ado, do, mata, sthlp, hlp, dlg, and smcl 
files for use in the Stata statistical package. It indents blocks 
of code properly, highlights command names, (most) keywords, some
more complicated command structures, strings, Stata macro names 
and the like.

ado-mode comes with a menu (the Ado-mode menu) which shows most all of the
variables which are worth changing locally in a buffer. Global customization
can be done via '\\[customize-group] ado' using emacs customization
routines. More suggestions can be found at 
http://homepage.mac.com/brising/Stata/ado-mode_install.html

Here is a short list of the common commands which come with the mode:
Things for dealing with files:
- \\[ado-new-ado] will make a new buffer ready for a new ado file.
- \\[ado-new-do] will make a buffer ready for a well-logged do file.
- \\[ado-new-help] will start a new help file, ready for editing.
- ado-mode can interact directly with Stata (Mac OS X and MS Windows only,
  for now) 
    \\[ado-send-command-to-stata] will send the current selection 
    to Stata for evaluation. If nothing is selected, the command containing
    the insertion bar will be sent.
- saving a buffer will save the current buffer and give it a good
    timestamp (if the ado-update-timestamp-flag is true, which it is
    by default). Ensures that the file name matches the name of the
    command (ado program) or class being defined.
    

Things for changing style:
Most of these would be most easily done using emacs' ability to customize
its enviroment using \\[customize-group ado-mode]. Other little things
 are
- \\[ado-tab-width-change] will change the tab-width for the current buffer.
- \\[ado-toggle-flag] which asks for the name of a flag to toggle. Even
    easier: use the Options... submenu of the Ado-mode menu..
Fixing up indentation
- \\[ado-indent-buffer] will re-indent the whole buffer.
Things for special Stata manipulations
- \\[ado-beginning-of-command] will move the point back to the beginning
    of the current command. If in the whitespace between two commands, it will
    move to the start of the next command.
- \\[ado-split-line] will split a long line in two using either the /* */ or ///
    style comments, depending on the value of ado-use-modern-split-flag (which
    defaults to on, implying the use of ///).
- \\[ado-foreach-loop] will insert a foreach loop, asking in the minibuffer
    for the particulars.
- \\[ado-forvalues-loop] will insert a forvalues loop, asking in the minibuffer for the particulars.
- \\[ado-insert-new-program] puts a new subprogram at the bottom of the current
    buffer for use within the current ado file.
- \\[ado-macify-selection-or-word] will turn the current selection or the word containing the point into a local macro.
- \\[ado-strmacify-selection-or-word] will turn the current selection or the word containing the point into a local macro enclosed in full string qualification.
- \\[ado-stringify-selection] will enclose the current selection with full string qualification.

Here an esoteric command which I've not yet documented well.
- \\[ado-new-label] will make a new label file useful for storing commonly
    used value labels.

Here are commands for sending code to Stata.
- \\[ado-send-command-to-stata] sends region or current command
- \\[ado-help-command] sends 'help current-command'
- \\[ado-help-at-point] sends 'help word-at-point'

Most all of the commands are accessible from the ado-mode menu.

If you also use ESS (Emacs Speaks Statistics), but you would rather
use this ado-mode to code Stata, include the following in your .emacs
file:

 (setq auto-mode-alist 
      (append (list '(\"\\\\.ado\\\\'\" . ado-mode)
		    '(\"\\\\.do\\\\'\"  . ado-mode)
		    )
	      auto-mode-alist
	      ))

This will make ado-mode load when you open an ado or do file."
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
  (setq indent-line-function 'ado-indent-line)
;  (make-local-variable 'indent-region-function)
;  (setq indent-region-function 'ado-indent-function)
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
  ;; !! just added
  (make-local-variable 'before-save-hook)
  (add-hook 'before-save-hook ado-before-save-file-hook)
  (use-local-map ado-mode-map)
  (setq mode-name "Ado")
  (setq major-mode 'ado-mode)
  ;; make sure function ends with lf
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'font-lock-defaults)
  (ado-set-font-lock-keywords)
  (setq font-lock-defaults '(ado-font-lock-keywords))
  (make-local-variable 'font-lock-syntactic-keywords)
  (ado-set-font-lock-syntactic-keywords)
  ;; make local copy of the extension, and try to guess the extension
  (make-local-variable 'ado-extension)
  (setq ado-extension (ado-find-extension))
  ;; setup directories which could be needed
  (unless ado-mode-home
	(setq ado-mode-home
		  (file-name-as-directory (expand-file-name (concat (file-name-directory (locate-file "ado-mode.el" load-path)) "..")))))
  (unless ado-site-template-dir
	(setq ado-site-template-dir (file-name-as-directory (concat ado-mode-home "templates"))))
  (unless ado-script-dir 
	(setq ado-script-dir (file-name-as-directory (concat ado-mode-home "scripts"))))
  (if ado-smart-indent-flag
      (if (or 
			  (string= ado-extension "hlp")
			  (string= ado-extension "sthlp")
			  (string= ado-extension "dlg")
			  (string= ado-extension "idlg"))
			 (setq ado-smart-indent-flag nil)
		  (if (string= ado-extension "do")
				(setq ado-smart-indent-flag ado-do-indent-flag)
			 )
		  )
    )
  ;; not a good idea --- since it is a bit heavy handed for custom indentations
  ;; (if ado-smart-indent-flag
  ;;    (ado-indent-buffer))
  (run-mode-hooks 'ado-mode-hook))

; ado-set-return == t -> swap ret and C-j
(defun ado-set-return (state)
  (if state
      (progn
		(define-key ado-mode-map "\C-m" 'ado-newline) 
		(define-key ado-mode-map "\C-j" 'newline) 
		)
    (define-key ado-mode-map "\C-j" 'ado-newline) 
    (define-key ado-mode-map "\C-m" 'newline) 
    ))

;;;; all the style-toggles for local resets rather than global

(defun ado-return-toggle ()
  (interactive)
  (setq ado-return-also-indents-flag (not ado-return-also-indents-flag))
  (ado-set-return ado-return-also-indents-flag))

(defun ado-toggle-flag (flag-name)
  (interactive "vWhat flag would you like to toggle? ")
  (set flag-name (not (eval flag-name))))

;;;; all the style value changers for local changes
;; a function which makes all the prompts and messages look the same.
(defun ado-change-number (variable newvalue)
  "For changing options which have numerical values somewhat nicely. Does
not work if fed an expression."
  (interactive "vWhat variable would you like to change? 
i")
  (if (or (null newvalue)
	  (eq newvalue 'ask))
      (progn 
	(setq newvalue (read-from-minibuffer (concat "Change " (symbol-name variable) " to ")  (number-to-string (eval variable) )))
	(if (or
	     (string= newvalue "")
	     (= (setq newvalue (string-to-number newvalue)) (eval variable)))
	    (progn
	      (message (concat "value of " (symbol-name variable) " left unchanged."))
	      nil)
	  (set variable (eval newvalue))
	  (message (format (concat "value of " (symbol-name variable) " set to %d.") (eval variable)))
	  t))
    (set variable (eval newvalue))
    t
    ))

(defun ado-tab-width-change (&optional new-tab-width)
"Changes the tab-width for the current buffer, and then optionally re-indents the file."
  (interactive)
  (if (not (ado-change-number 'tab-width new-tab-width))
      ()
    (if (y-or-n-p "Reindent buffer now? ")
	(progn
	  (save-excursion)
	  (ado-indent-buffer))
      )))

(defun ado-continued-statement-indent-spaces-change (&optional spaces)
"Changes the tab-width for the current buffer, and then optionally re-indents the file."
  (interactive)
  (if (not (ado-change-number 'ado-continued-statement-indent-spaces spaces))
      ()
    (if (y-or-n-p "Reindent buffer now? ")
	(progn
	  (save-excursion)
	  (ado-indent-buffer))
      )))

;;; scunged from the c-mode indentation
(defun ado-comment-indent ()
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
  "Indents continuation characters to a the \\[ado-continuation-column]. 
If there is no continuation on the current line, inserts the proper
continuation characters."
  (interactive)
  (let (cont-string cont-length (found-it t))
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
  (interactive)
  (error "This is out of date! Use a foreach loop (\\[ado-foreach-loop]), instead"))

(defun ado-foreach-loop (&optional macname listtype)
"Inserts a foreach loop, after asking for the type of loop to insert."
  (interactive)
  (if (not macname)
      (setq macname (read-from-minibuffer "What local macro should hold the tokens? ")))
  (if (not listtype)
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
"Inserts a forvalues loop, after asking for the range to insert."
  (interactive)
  (if (not macname)
      (setq macname (read-from-minibuffer "What local macro should hold the tokens? ")))
  (while (not range)
      (setq range (read-from-minibuffer "What numlist? ")))
  (ado-indent-line)
  (insert (concat "forvalues " macname " = " range ))
  (ado-insert-with-lfd " {")
  (ado-indent-line)
  (save-excursion 
	(newline-and-indent)
	(insert "}"))
  )

(defun ado-new-generic (type exten &optional stayput name purpose cusblp)
  "Allows overloading the new program to work for ado, class, do, mata and other files."
  (let (fullname buffullname (keepbuf t) (searchstr "startHere"))
	(unless name
	  (setq name (read-from-minibuffer (concat "What is the name of the " type "? "))))
	(setq fullname (concat name "." exten))
	(setq buffullname
;		  (switch-to-buffer (generate-new-buffer fullname)))
		  (set-buffer (generate-new-buffer fullname)))
	(ado-mode)
	(if cusblp
		(ado-insert-boilerplate cusblp nil t)
	  (ado-insert-boilerplate
	   (if (and (string= type "program") (string= exten "do"))
		   "testado.blp"
		 (concat exten ".blp"))))
	(unless purpose
	  (goto-char (point-min))
	  (if (search-forward "*!")
		  (setq purpose (read-from-minibuffer "What does it do? ")))
	  )
	(if (and (or ado-new-dir ado-personal-dir) (not stayput) (not (string= type "do-file")))
		(if ado-new-dir
			(if (y-or-n-p "Put in 'new' directory? ")
				(cd (directory-file-name ado-new-dir)))
		  (if ado-personal-dir
			  (if (y-or-n-p "Put in 'personal' directory? ")
				  (cd (directory-file-name ado-personal-dir)))
			)))
	(if (file-exists-p fullname)
		(setq keepbuf (y-or-n-p (concat "File " fullname " already exists! Overwrite?"))))
	(if keepbuf
		(progn
		  (if (string= ado-version-command "")
			(ado-reset-version-command))
		  (goto-char (point-min))
		  (while (search-forward "stata!!version" nil t)
			(replace-match ado-version-command))
		  (goto-char (point-min))
		  (if (search-forward "*!")
			  (progn
				(end-of-line)
				(insert (ado-nice-current-date))
				(if (search-forward "*!")
					(progn
					  (end-of-line)
					  (insert purpose)
					  ))))
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
			(ado-save-program))
		  )
	  (kill-buffer buffullname)
	  )
	))

(defun ado-new-do (&optional stayput name purpose)
  "Makes a new do-file by inserting the file do.blp from the template
directory. The do-file is made to keep its own named log so that it
can be called by other do-files."
  (interactive)
  (ado-new-generic "do-file" "do" stayput name purpose))

(defun ado-new-mata (&optional stayput name purpose)
  "Makes a new buffer by inserting the file mata.blp from the template
directory. Inserts the proper name for the new class and the class file
itself. Asks if the file should be saved in the `new' directory. If the
answer is no, the file will be saved in the current working directory.
Bound to \\[ado-new-mata]"
  (interactive)
  (ado-new-generic "mata file" "mata" stayput name purpose))

(defun ado-new-class (&optional stayput name purpose)
  "Makes a new buffer by inserting the file class.blp from the template
directory. Inserts the proper name for the new class and the class file
itself. Asks if the file should be saved in the `new' directory. If the
answer is no, the file will be saved in the current working directory.
Bound to \\[ado-new-class]" 
  (interactive)
  (ado-new-generic "class" "class" stayput name purpose)
  )

(defun ado-new-program (&optional stayput name purpose)
"Makes a new buffer by inserting the file ado.blp from the template
directory. Inserts the proper name for the new command and the ado file
itself. Asks if the file should be saved in the `new' directory. If the
answer is no, the file will be saved in the current working directory.
Bound to \\[ado-new-program]" 
  (interactive)
  (ado-new-generic "program" "ado" stayput name purpose))

(defalias 'ado-new-ado 'ado-new-program)

(defun ado-new-testado (&optional stayput name purpose)
"Makes a new buffer by inserting the file testado.blp from the template
directory. The templay is a do-file which -includes- an ado-file by the
same name for easier debugging. Asks if the do-file should be saved in 
the `new' directory. If the answer is no, the file will be saved in the 
current working directory.
Bound to \\[ado-new-testado]" 
  (interactive)
  (ado-new-generic "program" "do" stayput name purpose))

(defun ado-marker-program ()
  (interactive)
  (let
      ((long-name (read-from-minibuffer "What is the name of the condition? "))
       (short-name (read-from-minibuffer "What is the default name of the marker? "))
       program-name)
    (setq program-name
	  (concat "_mk" (substring short-name 0 (min 5 (length short-name)))))
    (ado-new-program nil program-name
		       (concat "Generates default marker " short-name " for the condition " long-name))
    (ado-insert-boilerplate "markit.blp")
    (re-search-forward "\"\"")
    (forward-char -1)
    (insert long-name)
    (re-search-forward "()")
    (forward-char -1)
    (insert short-name)))

(defun ado-project-program ()
  (interactive)
  (ado-new-program)
  (ado-insert-boilerplate "proj1.blp")
  (goto-char (point-max))
  (forward-line -2)
  (ado-insert-slog-block)
  (ado-insert-boilerplate "proj2.blp")
  (goto-char (point-min))
  (search-forward "stages \"")
  (recenter))

(defun ado-stage-program ()
  (interactive)
  (ado-new-program t)
  (ado-insert-slog-block "replace")
  (ado-insert-boilerplate "stage.blp")
  ;(search-forward "tmp" nil t)
  )

(defun ado-lookup-program ()
  (interactive)
  (ado-new-program)
  (ado-insert-boilerplate "lookup.blp")
  (goto-char (point-min))
  (search-forward "using \""))

(defun ado-checker-setup ()
  (interactive)
  (let ((var-name (read-from-minibuffer "What is the name of the variable? "))
	err-name)
    (ado-new-program var-name nil (concat "Error checker for " var-name))
    var-name))

(defun ado-checker-err-name (var-name)
  (concat "err" (substring var-name 0 (min 5 (length var-name)))))

(defun ado-ckmiss-program (&optional name)
  (interactive)
  (let ((var-name (ado-checker-setup))
	err-name)
    (setq err-name (ado-checker-err-name var-name))
    (ado-insert-boilerplate "ckmiss.blp")
    (while (search-forward "foovar" nil t)
      (replace-match var-name nil t))
    (while (search-forward "fooerr" nil t)
      (replace-match err-name nil t))
    ))

(defun ado-ckdunno-program (&optional name)
  (interactive)
  (let ((var-name (ado-checker-setup)))
    (ado-insert-boilerplate "ckdunno.blp")
    (while (search-forward "foovar" nil t)
    (replace-match var-name nil t))
    ))

(defun ado-checker-program ()
  (interactive)
  (let ((var-name (ado-checker-setup))
	err-name)
    (setq err-name (ado-checker-err-name var-name))
    (ado-insert-boilerplate "check.blp")
    (while (search-forward "foobar" nil t)
      (replace-match var-name nil t))
    (goto-char (point-min))
    (while (search-forward "errfoobar" nil t)
      (replace-match err-name nil t))
    (goto-char (point-min))
    (search-forward "=")
    ))
  
(defun ado-insert-new-program (&optional name purpose)
  "Inserts a subprogram at the bottom of the current buffer. There is
something broken in that the insertion point is left in the wrong spot..."
  (interactive)
  (if (not name)
      (setq name (read-from-minibuffer "What is the name of the program? ")))
  (goto-char (point-max))
  (ado-insert-boilerplate "smallado.blp")
  (search-forward "program define")
  (end-of-line)
  (insert name)
  (search-forward "	"))

(defun ado-new-label (&optional name)
  "Grab the boilerplate for a label and name the buffer"
  (interactive)
  (if (not name)
      (setq name (read-from-minibuffer "What is the name of the label? ")))
  (switch-to-buffer
   (generate-new-buffer
    (generate-new-buffer-name (concat name ".lbl"))))
  (ado-insert-boilerplate "lbl.blp")
  (if (and ado-label-dir (y-or-n-p "Put in local 'lbl' directory? "))
      (cd (directory-file-name ado-label-dir))
    (pwd))				;put in to avoid troubles later!
  (ado-mode)
  (goto-char (point-min))
  (re-search-forward "def ")
  (insert name)
  (forward-char 1))

(defun ado-write-file-as-buffer-name ()
  "Takes care of the problem in emacs where a buffer can have its name 
changed, but will write itself under it's regular filename. Not used anywhere?"
  (interactive)
  (let (this-buffer)
    (setq this-buffer (buffer-name))
    (if (string-match "*" this-buffer)
	(save-buffer)
      (write-file (substring this-buffer 0 (string-match "<" this-buffer)) ado-confirm-overwrite-flag)
      ))
  )

(defalias 'ado-save-program 'save-buffer
  "ado-save-program is obsolete as a special function.
Use the proper combination of a before-save-hook and
\\[save-buffer] to save things nicely."
)

;;   (setq ado-extension (ado-find-extension))
;;   (if ado-update-timestamp-flag
;; 	  (ado-update-timestamp))
;;   ;; if file name specified, this is just a write-file
;;   (unless filename
;; ;;	  (write-file filename ado-confirm-overwrite-flag)
;; 	;; try to get new name
;; 	(setq filename (ado-make-ado-name)))
;;   (if filename
;; 	  (unless (string= (concat default-directory filename) (buffer-file-name))
;; 		(set-visited-file-name filename)
;; 		(if (and ado-confirm-overwrite-flag
;; 				 (file-exists-p (buffer-file-name)))
;; 			(unless (y-or-n-p (concat "Overwrite file " filename "? "))
;; 			  (error "Canceled"))
;; ;;		(write-file filename ado-confirm-overwrite-flag)
;; 		  )))
;;   (save-buffer))

(defun ado-before-save-file ()
  "The default before-save-hook. This updates the timestamp using
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
			  (error "Canceled"))
;;		(write-file filename ado-confirm-overwrite-flag)
			)))))
  

(defun ado-update-timestamp ()
  "Tries to do a nice job updating a timestamp for the file. Since
Stata has no conventions about headers for files, ado-mode will:

Look for a '*! version xxx' statement in ado/do/ files, a {
* version xxx } or a {* <date>}{...} in sthlp (or hlp)
files, or a version x.y.z <date> in other files."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (or (string= ado-extension "ado") 
			(string= ado-extension "class") 
			(string= ado-extension "do")) 
		(if (re-search-forward "^[*]![ \t]+version[ \t]+[0-9\.]*[ \t]*" (point-max) t)
			(progn
			  (delete-region (point) (point-at-eol))
			  (insert (ado-nice-current-date))
			  ))
      (if (or
		   (string= ado-extension "hlp")
		   (string= ado-extension "sthlp"))
		  (if (search-forward "[*] Last Updated: " (point-max) t)
			  (progn
				(delete-region (point) (point-at-eol))
				(insert (ado-nice-current-date))
				(insert "}{...}"))
			(if (re-search-forward "^[ \t]*{[*]+[ \t]+[*]![ \t]+version[ \t]+[0-9\.]*[ \t]*" (point-max) t)
				(progn
				(delete-region (point) (point-at-eol))
				  (insert (ado-nice-current-date))
				  (insert "}{...}"))
			  (if (looking-at "{smcl}[ \t]*")
				(progn
				  (goto-char (match-end 0))
				  (forward-char)
				  (if (looking-at "{[*] *")
					  (progn 
						(goto-char (match-end 0))
						(delete-region (point) (point-at-eol))
						(insert (ado-nice-current-date))
						(insert "}{...}")))))))
	;;; not in ado or help file
		(if (re-search-forward "^\\([*]!\\)*[ \t]+[Vv][Ee][Rr][Ss][Ii][Oo][Nn][ \t]+[0-9\.]*[ \t]*" (point-max) t)
			(progn
			  (delete-region (point) (point-at-eol))
			  (insert (ado-nice-current-date))
			  ))))))
	
    ;; looking for a version number, so that the date stamp can be updated
;; leaves the new date and time, even if the file is not saved... dunno what
;; to do to fix this, since I cannot find error trapping for errors which do
;; not use 'signal to identify the errors (write-file does not use a signal)

(defun ado-set-ado-extension ()
  "This function is obsolete. Try using \\[ado-find-extension] 
instead, which returns a value instead of setting a variable."
  (interactive)
  (setq ado-extension (ado-find-extension)))

(defun ado-show-extension ()
  "Shows what ado-mode thinks the file extension for the current
buffer should be by running \\[ado-find-extension]."
  (interactive)
	(message (concat "I think the extension is " (ado-find-extension))
	))


(defun ado-find-extension (&optional message)
  "Decides from the file contents what the extension should be. Since Stata
has started getting more complicated, will fall back to the current file
if confused. Returns its best guess at the extension.

To test this, try \\[ado-show-extension]."
  ;; try to find the name, trust the buffer if lost, but issue warning if
  ;; lost or a contradiction
  (let (first-program first-mata sez-file sez-contents)
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
			(message (concat "ado-mode thinks that the extension should be " sez-contents " even though the current extension is " sez-file "!")))
		  sez-file)
	  sez-contents)
	))

(defun ado-show-ado-name ()
  "Shows what ado-mode thinks the name of the edited file should be by
running \\[ado-make-ado-name]."
  (interactive)
  (message (ado-make-ado-name))
  )
						  
(defun ado-make-ado-name ()
  (interactive)
  "Creates a file name from the contents of the file. Assumes that
ado-extension has been set properly. (See \\[ado-find-extension] for 
determining extensions.) Returns nil if the name of the file
cannot be determined from the file contents. You can test this using
\\[ado-show-ado-name].

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
    file starts with *! in the first line, ado-mode will check
    lines starting with *!'s at the top of the program for something
    approaching a filename with the proper extension. If it is found,
    it will be used as the file name."
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
		  (re-search-forward "[a-zA-Z_]+[a-zA-Z0-9_]*\\b")
		  (setq name-end (point))
		  (concat (buffer-substring-no-properties name-start name-end) "." ado-extension))
		))))

(defun ado-make-help-name ()
  (interactive)
  "Creates a file name from the contents of a help file, assuming that
the ado-extension has been set properly. Throws an error if the name cannot be
determined. This was split from \\[ado-make-file-name] because of big changes
to help files in Stata 12 (and the initial buggy fix)."
  (let ((debug-on-error t) name-start name-end full-name) ; titlepos syntaxpos (name-start nil))
	(save-excursion
	  (goto-char (point-min))
	  (cond 
	   ((search-forward-regexp "{manlink[ \t]+.*?[ \t]+\\(.*?\\)[ \t]*}" nil t)
		(setq full-name (mapconcat 'identity (split-string (match-string-no-properties 1 nil)) "_")))
	   ((re-search-forward "{\\(bf\\|cmd\\|hi\\):[ \t]*help[ \t]+\\(.*?\\)[ \t]*}" nil t)
		(setq full-name (mapconcat 'identity (split-string (match-string-no-properties 2 nil)) "_")))
	   ((re-search-forward "{\\(bf\\|cmd\\|hi\\):[ \t]*\\(.*?\\)[ \t]*}" nil t)
		(setq full-name (mapconcat 'identity (split-string (match-string-no-properties 2 nil)) "_")))
	   ((search-forward "help for " nil t) ; very old help
		(re-search-forward "{\\(bf\\|cmd\\|hi\\):[ \t]*\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)\\b" nil t)
		(setq full-name (mapconcat 'identity (split-string (match-string-no-properties 1 nil)) "_")))
	   (t (error "Could not figure out help file name!"))
	   )
	  )
	(concat full-name "." ado-extension))
  )
  
(defun ado-find-help-name-start ()
  "Returns the point at which the command documented in a help file starts.
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
	   (t nil))
	  )))


(defun ado-find-help-name-start-defunct ()
  "Returns the point at which the command documented in a help file starts.
Need a function for this, because this location changed drastically between
Stata versions 11 and 12."
  (interactive)
	;; first find where Title and Syntax are, since these are bounds for searches
  (let ((debug-on-error t) titlepos syntaxpos (name-start nil))
	(save-excursion
	  (goto-char (point-min))
	  (setq titlepos (search-forward "Title" nil t))
	  (if titlepos
		  (progn
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
					(re-search-forward "{cmd:[ \t]*" syntaxpos t)))
			)))
	name-start
	))


(defun ado-find-local-name ()
  "Returns the name of the defining program in which the point is sitting. If the
program define statement cannot be found, returns nil. Returns a mess if the
program name is missing (need to fix this). Currently broken --- need to assess need."
  (let (name-start name-end)
    (save-excursion
      (if (re-search-backward "^pr\\(o\\|\\og\\|\\ogr\\|\\ogra\\|\\ogram\\)[ \t]+" 0 t) ;goes to most recent definition
	  (progn
	    (setq name-start (re-search-forward 
			"^pr\\(o\\|\\og\\|\\ogr\\|\\ogra\\|\\ogram\\)[ \t]+\\(de\\(f\\|fi\\|fin\\|fine\\)[ \t]+\\)*" nil t)
		  name-end (re-search-forward "[a-zA-Z_]+[a-zA-Z0-9_]*\\b"))
	    (buffer-substring-no-properties name-start name-end))
	))))

(defun ado-show-local-name ()
  (interactive)
  (message (concat "The local program is " (ado-find-local-name))))

(defun ado-insert-file-and-indent (file)
  "Interactively insert (and properly indent) an ado file.
Can also be called from within another program for inserting
and indenting"
  (interactive "fGive the file to insert: ")
  (let ((beg (point)))
    (insert-file-contents file)
    (ado-indent-region beg (point))
    ))

(defun ado-insert-boilerplate (file-name &optional raw full-path)
  (if full-path
	  (setq full-path file-name)
	(if (not ado-site-template-dir)
		(error "%s" "Use \\[set-variable] to set ado-site-template-dir to the directory holding the ado templates!"))
	(setq full-path (concat (file-name-as-directory ado-site-template-dir) file-name)))
  (if raw
      (insert-file-contents full-path)
    (ado-insert-file-and-indent full-path)))


(defun ado-insert-slog-block (&optional replace-flag)
  (interactive)
  (let ((the-name (ado-find-local-name)))
    (ado-insert-boilerplate "slog.blp")
    (re-search-forward "\"\"")
    (forward-char -1)
    (if the-name
	(insert the-name)
      (error "Could not determine the name of the defining program"))
    (if replace-flag
	(progn
	  (re-search-forward "append")
	  (forward-word -1)
	  (kill-word 1)
	  (insert "replace")))
    (re-search-forward "{")
    (newline-and-indent)
    ))

;;; depth and indentation commands
(defun ado-line-starts-with-end-comment ()
  (interactive)
  (save-excursion
	(beginning-of-line)
	(and (search-forward "*/" (point-at-eol) t)
		 (not (re-search-forward "[ /t]*$" (point-at-eol) t))
		 (not (search-backward "/*" (point-at-bol) t)))))

(defun ado-out-of-nested-comment (&optional top from-level)
  (interactive)
  (let ((ppsexp (parse-partial-sexp 1 (point))) this-level )
    (if (numberp (setq this-level (nth 4 ppsexp)))
	(if (or (not from-level) (and (<= from-level this-level)))
	    (if (search-backward "/*" 1 t)
		(if top (ado-out-of-nested-comment t)
;		  (forward-char -1)
		  (ado-out-of-nested-comment nil this-level)
		  ))
;	  (if (not top) (forward-char 1))
	  )
      (if top
	  (if (search-backward "*/" (point-at-bol) t)
	      (ado-out-of-nested-comment top)
	    )))))

(defun ado-show-depth ()
  (interactive)
  (let ((depth (ado-find-depth)))
    (message (concat "The depth was " (number-to-string (car depth))
		     (if (nth 1 depth) " with continuation" " without continuation")))))

;; changed left-indenting of 'version' to left-indent iff spelled out
;;   to avoid the indenting, use an abbreviation
(defun ado-find-depth ()
  (let (depth start ppsexp in-continuation (oddend (ado-line-starts-with-end-comment)))
    (save-excursion
      ;; look for line starting with a comment
;;bug;; (save-excursion
		(setq in-continuation (ado-beginning-of-command))
;;bug;;		)
      (setq ppsexp (parse-partial-sexp 1 (point)))
      (setq depth (car ppsexp))
      (setq start (point))
      ;; oddities which might need unindenting
      (when (or oddend
				(and (not ado-close-under-line-flag) (looking-at "}"))
				(looking-at "ver\\(s\\|si\\|sio\\|sion\\)"))	  
		(setq depth (1- depth)))
	  (end-of-line)
	  (setq depth (- depth (how-many "^[ \t]*\\(end$\\|end[ \t]+\\)" 1 (point))))
	  ;;      (setq depth (- depth (how-many "^[ \t]*end$" 1 (point))))
	  (beginning-of-line)
      ;; words which start blocks
	  ;; need to be careful, because of program dir, drop, and list
      (setq depth (+ depth (how-many "^[ \t]*\\(input[ \t]+\\|\\(p\\(r\\|ro\\|rog\\|rogr\\|rogra\\|rogram\\)\\([ \t]+d\\(ef\\|efi\\|efin\\|efine\\)\\)?\\)[ \t]+\\|\\(mat\\(a\\|a:\\)\\)[ \t]*$\\)" 1 (point))))
	  (setq depth (- depth (how-many "^[ \t]*p\\(r\\|ro\\|rog\\|rogr\\|rogra\\|rogram\\)[ \t]+\\(d\\(i\\|ir\\)[ \t]*$\\|\\(\\(l\\|li\\|lis\\|list\\|drop\\)[ \t]+\\)[a-zA-Z_]+\\)" 1 (point))))
      ;; words which end blocks
      (setq ppsexp (parse-partial-sexp start (point)))
      (if (numberp (nth 4 ppsexp))
	  (list (+ depth (nth 4 ppsexp)) in-continuation)
	(list depth in-continuation)))
      ))

(defun ado-indent-region (&optional start end)
  (interactive)
  (let (endmark)
    (if (and (null start) (null end))
	(progn
	  (setq start (min (point) (mark)))
	  (setq end (max (point) (mark))))
      )
    (save-excursion
      (goto-char start)
      ;; Advance to first nonblank line.
      (beginning-of-line)
      (setq endmark (copy-marker end))
      (while (and (bolp) (not (eobp)) (< (point) endmark))
	(skip-chars-forward " \t\n")
	(ado-indent-line)
	(forward-line 1)
      ))))

(defun ado-indent-buffer ()
  (interactive)
  (save-excursion
    (ado-indent-region (point-min) (point-max))))

(defun ado-indent-line ()
  "A smart indenter for ado files. Many of the parameters can
be customized using '\\[customize-group] ado-mode'."
  (interactive)
  (if ado-smart-indent-flag
      (let (indent depth beg shift-amt endmark
	    (pos (- (point-max) (point)))
	    (watch-for-semi (ado-delimit-is-semi)))
	(beginning-of-line)
	(setq beg (point))
	(cond ((and ado-delimit-indent-flag (looking-at "[ \t]*#d\\(e\\|el\\|eli\\|elim\\|elimi\\|elimit\\)"))	;#delimits belong at delimit indent
	       (setq indent ado-delimit-indent-column))
	      ((and ado-comment-indent-flag
		    (or (looking-at "^\\*") (looking-at "^*")))	;comments at start of line belong at comment indent
	       (setq indent ado-comment-indent-column))
	      ((and ado-debugging-indent-flag
		    (or (looking-at "^[ \t]*pause")	
			(looking-at "^[ \t]*set t\\(r\\|ra\\|rac\\|race\\)[ \t]+")))
	       (setq indent ado-debugging-indent-column)) ; debugging at proper column (usually 0)
	      (t (setq indent (* tab-width (car (setq depth (ado-find-depth)))))   ; regular indentation
			 (if (nth 1 depth)
				 (setq indent (+ indent ado-continued-statement-indent-spaces)))
			 ))						  ; end of conditional statement
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

(defun ado-delimit-is-semi ()
  "Returns t if semicolons delimit commands, otherwise returns nil."
  (save-excursion
    ;; if #delimit command is on same line, the delimiter is always cr
    (let ((line-start (point-at-bol)))
      (if (re-search-backward "#[ \t]*d\\(e\\|el\\|eli\\|elim\\elimi\\elimit\\)" 1 t)
		  (let ((ppsexp (parse-partial-sexp 1 (point))))
			(if (or 
				 (nth 3 ppsexp)		; inside a string
				 (nth 4 ppsexp)		; inside a non-nestable comment
				 (nth 7 ppsexp))		; inside a type-b comment
				(ado-delimit-is-semi)
			  (if (>= (point) line-start)
				  nil
				(forward-sexp)
				(skip-chars-forward " \t")
				(looking-at ";"))))))))

(defun ado-show-delimiter ()
  "Returns the value of the delimiter in ado-functions as a message."
  (interactive)
  (message (if (ado-delimit-is-semi)
	       "The delimiter is ;"
	     "The delimiter is cr"
	     )))

(defun ado-beginning-of-command ()
  "Moves the cursor to the start of the command in which the insertion
point is sitting. This will jump back to the start of a command if the
insertion point is within the command, and jump forward to the start
of a command if the delimiter is in whitespace preceding a command.

When the delimiter is cr, blank lines count as empty commands, so the
cursor will stay put.

When the delimiter is ; even being just after a ; will mean that
the cursor is after the command, and will cause a jump forward. This is
technically correct, even if a bit disturbing.

This is known to have trouble when there are bizarre constructions, such
as /* */-style comments in the middle of a line, or something perverse like
   regress mpg // this is a comment /// with a fake continuation
     weight
Fixing this might or might not happen in the future.

Returns t if inside of a continued function, nil otherwise."
  (interactive)
  (let ((in-continuation nil) (start-line (line-number-at-pos)) (skip-chars " \t"))
    (ado-out-of-nested-comment t)
    ;; first look backwards for either delimiter or delimit command
	(if (= start-line 1)
		(beginning-of-line)
	  (unless (search-backward "#delimit" (point-at-bol) t)
		(if (ado-delimit-is-semi)
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
    in-continuation
    ))

(defun ado-end-of-command ()
  "Move to the end of the current command. This can be fooled
by /* */-style commands extending across lines."
  (interactive)
  (let (ppsexp)
    (if (ado-delimit-is-semi)
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
			  (if (not (< (point) (point-max)))
				  (error "No end of command - moved as far as possible")
				(forward-char)
				(ado-end-of-command)))
		  (if (not (search-forward "//" (point-at-eol) t))
			  (end-of-line)
			(backward-char 2)
			(skip-syntax-backward "-")))
		))))

(defun ado-copy-command (&optional asString)
  "Copies the command in which point is sitting to the clipboard.
May be called interactively, but really meant for using within items
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
		 (x-select-enable-clipboard t))
	(if asString
		(buffer-substring-no-properties start-here end-here)
	  (kill-ring-save start-here end-here))
	))

;; stolen from c-mode, and changed slightly, since Stata does not allow
;; braces on separate lines
(defun electric-ado-closing-brace (arg)
  "Insert closing character, possibly on new line, and correct line's indentation."
  (interactive "P")
  (if (and (not arg)
	   (eolp)
;	   ado-auto-newline-flag
	   ado-closing-brace-alone-flag)
      (if (save-excursion
	    (skip-chars-backward " \t")
	    (not (bolp)))
	(newline-and-indent)))
  (electric-ado-brace arg))

(defun electric-ado-brace (arg)
  "Insert character and correct line's indentation."
  (interactive "P")
  (let (insertpos)
    (if (and (not arg)
	     (eolp)
	     ado-auto-newline-flag)
	(progn
	  (insert last-command-event)
	  (ado-indent-line)
	  (if ado-auto-newline-flag
	      (progn
		(newline)
		;; (newline) may have done auto-fill
		(setq insertpos (- (point) 2))
		(ado-indent-line)))
	  (save-excursion
	    (if insertpos (goto-char (1+ insertpos)))
	    (delete-char -1))))
    (if insertpos
	(save-excursion
	  (goto-char insertpos)
	  (self-insert-command (prefix-numeric-value arg)))
      (self-insert-command (prefix-numeric-value arg)))))

(defun ado-newline ()
  "Justifies current line before doing a newline-and-indent"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (ado-indent-line))
  (newline-and-indent))

(defun ado-split-line ()
  "Splits line at point, putting in the proper continuation
characters, depending on the value of \\[ado-use-modern-split-flag]"
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
		(insert "*/"))  
	 ))

(defun ado-macify-selection-or-word (&optional stringify)
  "Puts local macro markers `' around the word in which the point sits, leaving point after word. If point is not in a word, simply inserts `'."
  (interactive)
  (let (noregion popmark)
	(unless (and transient-mark-mode mark-active)
;;	(unless mark-active
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
	  
	  (save-excursion (goto-char (region-beginning)) 
							(if stringify (insert "`\""))
							(insert "`"))
	  (goto-char (region-end)) 
	  (insert "'")
	  (if stringify (insert "\"'"))
	  (if popmark	(pop-mark))
	  )))

(defun ado-strmacify-selection-or-word ()
  (interactive)
  (ado-macify-selection-or-word t))

(defun ado-stringify-selection ()
  "Puts full string delimiters around the selection. If nothing is selected, inserts full string delimiters at point."
  (interactive)
  (if (and transient-mark-mode mark-active)
		(progn 
		  (save-excursion (goto-char (region-beginning)) (insert "`\""))
		  (goto-char (region-end)) (insert "\"'"))
	 (insert "`\"\"'")
	 (forward-char -2)))

(defun electric-ado-semi (arg)
  "Puts in extra newline if the semi-colon is the end-of-line delimiter"
  (interactive "P")
  (if (ado-delimit-is-semi)
      (electric-ado-brace arg)
    (self-insert-command (prefix-numeric-value arg))))

;; here comes all the stuff for auto-highlighting!
;;


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

;;; now for fancy stuff: syntactic keywords
(defun ado-set-font-lock-syntactic-keywords ()
  (interactive)
  (setq font-lock-syntactic-keywords
		  (list
			'("\\(#\\)[dr]" 1 "w")
			)))



;;; Working with help files
;;; allowing for the user's name to be put into help files
(defun set-ado-signature-file ()
  (interactive)
  (setq ado-signature-file
		(read-file-name "Set ado signature file to: "
		   (file-name-directory 
			(expand-file-name
			 (if (not ado-signature-file)
				 (if (file-exists-p "~/.ado-signature")
					 "~/.ado-signature")
			   ado-signature-file)))))
  )

(defun ado-new-help-6 ()
  "Gets a boilerplate for writing help files for Stata 6 and earlier, then inserts the users name and allows editing. This is not complete, since it has trouble putting the proper signature at the bottom of the file. To make this work properly, have a variable which contains the name of the signature file or a .signature file."
  (interactive)
  (let (name)
    (setq name (read-from-minibuffer "For what program should help be written? "))
    (switch-to-buffer
     (generate-new-buffer
      (generate-new-buffer-name (concat name ".hlp"))))
    (ado-insert-boilerplate "help6.blp" t)
    (if (and ado-new-dir (y-or-n-p "Put in 'new' directory? "))
		  (cd (directory-file-name ado-new-dir)))
    (if (not ado-claim-name)
		  (setq ado-claim-name 
				  (read-from-minibuffer "Whose name should be put at the top of the file? "
												user-full-name)))
    (text-mode)
    (goto-char (point-max))
    (if ado-signature-prompt-flag
		  (progn
			 (if (not ado-signature-file)
				  (set-ado-signature-file))
			 (insert-file-contents ado-signature-file))
      (insert ado-claim-name))
    (goto-char (point-min))
    (forward-line 1)
    (end-of-line)
    (insert (concat "^" name "^"))
    (insert (concat
				 (make-string
				  (- 81 (+ (length ado-claim-name) (current-column)))
				  (string-to-char " "))
				 ado-claim-name))
    (search-forward "Put a")
    (beginning-of-line)
    (set-fill-column 79)
    (auto-fill-mode 0)
    (recenter)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; stuff for writing help files ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ado-new-help (&optional name)
  "Gets a template for writing help files, inserts the users name and allows editing. To make this work properly, have a variable which contains the name of the signature file or a .signature file."
  (interactive)
  (unless name
    (setq name (read-from-minibuffer "For what program should help be written? ")))
  (switch-to-buffer
   (generate-new-buffer
	(generate-new-buffer-name (concat name ado-help-extension))))
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
					(set-ado-signature-file)))
			(insert-file-contents ado-signature-file)
		  ;; no signature file
		  (if (not ado-claim-name)
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
	  (delete-region (point) end-here))
	)
  (ado-mode)
  ;; turn off indenting
  (setq ado-smart-indent-flag nil)
  (ado-save-program)
  (goto-char (point-min))
  (search-forward "title of command")
  (beginning-of-line)
  )

(defun ado-toggle-help-extension ()
  "Toggles the extension between sthlp and hlp for those writing
help files meant to be used in Stata 7 through Stata 9."
  (interactive)
  (if (string= ado-help-extension "sthlp")
	  (setq ado-help-extension "hlp")
	(setq ado-help-extension "sthlp"))
  (message (concat "ado-help-extension is now " ado-help-extension)))

;;; useful insertions in smcl
(defun ado-help-insert-option-in-body (&optional option-name)
  (interactive)
  (if (not option-name)
      (setq option-name
				(read-from-minibuffer "What is the full name of the option? " option-name)))
  (ado-insert-with-lfd (concat "{p 0 4}{cmd:" option-name "}"))
  (ado-insert-with-lfd "{p_end}")
  (newline)
  (forward-line -2)
  (end-of-line)
  (forward-char -1)
  )

(defun ado-new-cscript (&optional desc name)
  "Gets a boilerplate for writing certification scripts, asks for the ado file being certified, and then allows editing."
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
  (ado-mode)
  )


;;; Some utilities which should really be in a separate file (but which
;;;  would then cause extra installation instructions).

(defun ado-reset-value (value prompt &optional flag)
  (let (new-value value)
    (setq new-value (read-from-minibuffer (format (concat "Change " prompt " from %d to ") value)))
    (if (null new-value)
		  value
      (if (= (setq new-value (string-to-number new-value )) value)
			 (progn 
				(message (concat prompt " left unchanged."))
				value)
		  new-value
		  )
      )))

(defun ado-insert-with-lfd (junk)
  "Used to insert and indent without needed to hit the indentation key
  (usually a tab). One day, when the ado-mode is complete, the other
  functions will no longer depend on this function."
  (insert junk)
  (newline-and-indent))

(defun ado-nice-current-date ()
  "Returns the current date and time as specified by by
  `ado-date-format'. See `format-time-string' for help on setting."
  (concat 
   (if ado-lowercase-date-flag
       (downcase (format-time-string ado-date-format))
     (format-time-string ado-date-format))
   (if ado-initials-flag (concat " " ado-initials))))

(defun ado-insert-nice-current-date ()
  (interactive)
  "Inserts a the nice current date (see \\[ado-nice-current-date] for
details"
  (insert (ado-nice-current-date)))

(defun ado-set-imenu-items ()
  (interactive)
  (setq imenu-case-fold-search nil)
  (setq imenu-generic-expression
		(list 
		   (list nil "^\\s-*pr\\(o\\|og\\|ogr\\|ogra\\|ogram\\)\\(\\s-+\\(de\\|def\\|defi\\|defin\\|define\\)?\\)\\s-+\\([a-zA-Z_][a-zA-Z_0-9]*\\)" 4))))

;; for finding lists of directories where Stata has files
(defun ado-find-ado-dirs (&optional dir subdir)
  (interactive)
  (unless dir
	(setq dir "/Universal/Custom/Stata/ado/Downloads"))
  (unless subdir
	(setq subdir "all"))
  (append 
   (if (or (string= subdir "all") (string= subdir "self"))
	   (list dir))
   (if (or (string= subdir "all") (string= subdir "sub"))
	   (directory-files dir t "^[a-z_0-9]$"))
   nil
   )) 

(defun ado-next-error (&optional goback)
  (interactive)
  "Looks for next error in a log file (smcl or txt). If the optional 
argument goback is non-nil, looks backwards (see also ado-prev-error). 
If looking through a file where tracing is on, goes back to the 
line which caused the error."
  (let (whereto traceoff traceon)
	(save-excursion
	  (if goback
		  (setq whereto (re-search-backward "^\\({err}\\|^r([1-9][0-9]*)\\)" nil t))
		(setq whereto (re-search-forward "^\\({err}\\|^r([1-9][0-9]*)\\)" nil t))
		))
	(if whereto
		(progn
		  (goto-char whereto)
		  (beginning-of-line)
		  (save-excursion
			(setq whereto (re-search-backward "^[ \t]*\\(-\\|=\\)" nil t)))
		  (if whereto
			  (progn
				(save-excursion
				  (setq traceoff (search-backward "set trace off" nil t)))
				(if traceoff
					(progn
					  (save-excursion
						(setq traceon (search-backward "set trace on" nil t)))
					  (if (< traceon traceoff)
						  (if (< traceoff whereto)
							  (goto-char whereto))))
				  (goto-char whereto)))
			))
	  (message (concat "no "
					   (if goback
						   "previous"
						 "next")
					   " error found"))
	)))

(defun ado-prev-error ()
  (interactive)
  (ado-next-error t))

;; matching parens
(defun ado-balance-brace (&optional block)
  "Select whatever is inside balanced braces -{[()]}- but outside strings
or comments. If block is non-nil, balance only {} in a smart way, 
being sure to include loop-inducing commands."
  (interactive)
  (let (here there (ppsexp (parse-partial-sexp 1 (point))))
	(save-excursion
	  ;; jump to start of comment/string so scan-lists works
	  (if (or (nth 3 ppsexp) (nth 4 ppsexp))
		  (goto-char (nth 8 ppsexp)))
	  ;; if no block, these error out
	  (condition-case nil
		  (setq here (scan-lists (point) -1 1))
		(error (error "Beginning brace not found")))
	  (condition-case nil
		  (setq there (scan-lists (point) 1 1))
		(error (error "Ending brace not found")))
	  )
	;; jump to start of loop if needed
	(goto-char here)
	(set-mark there)
	(unless (not block)
	  (if (looking-at "[[(]")
		  (setq there (ado-balance-brace t))
		(ado-beginning-of-command)
		(unless (looking-at "[\t ]*\\(\\(for\\(each\\|\\(v\\|va\\|val\\|valu\\|value\\|values\\)\\)\\)\\|while\\)[\t ]+")
		  (goto-char here))
	  ))
  ))

(defun ado-grab-block ()
  (interactive)
  (ado-balance-brace t))

(defun ado-send-block-to-stata ()
  (interactive)
  (ado-grab-block)
  (ado-send-command-to-stata))

(defun ado-strip-after-newline (theString)
  (interactive)
  (if (string-match "\n.*" theString) 
	  (replace-match "" nil nil theString)
	theString)
  )

(defun ado-skip-special-comments ()
  "Skips *! comments and empty lines from the current line until 
they run out. Used to find the beginning of programs."
  (interactive)
  (goto-char (point-at-bol))
  (while (re-search-forward "^\\([*]!\\|[ \t]*$\\)" (point-at-eol) t)
	(forward-line)
	(goto-char (point-at-bol))))

(defun ado-skip-header-lines ()
  "Skips * and *! comments, empty lines, and capture program drop lines 
from the current line until they run out. Used to find the beginning of 
programs, even those defined in a funky way."
  (interactive)
  (goto-char (point-at-bol))
  (while (and (< (point) (point-max))
	(goto-char (point-at-bol))
	(re-search-forward "^[ \t]*\\([*]\\|$\\|vers\\|versi\\|version\\|\\(ca\\(p\\|pt\\|ptu\\|ptur\\|pture\\)[ \t]+p\\(r\\|ro\\|rog\\|rogr\\|rogra\\|rogram\\)[ \t]+drop\\)\\)" (point-at-eol) t))
	(forward-line)
	))

(defun ado-statacorp-defaults ()
  "Sets editing options to StataCorp default stamps and such for editing"
  (interactive)
  (set-variable 'ado-close-under-line-flag nil)
  (set-variable 'ado-lowercase-date-flag t)
  (set-variable 'ado-date-format "%d%b%Y")
  (set-variable 'ado-initials-flag nil)
  (message "ado-mode options set to StataCorp defaults"))

			 
;;; Aquamacs emacs specifics (perhaps should be broken out?)
(if (boundp 'aquamacsxb-version)
    (define-key ado-mode-map [remap mac-key-save-file] 'ado-save-program))



(provide 'ado-mode)
;; ado-mode.el ends here
